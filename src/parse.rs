#![allow(unused_imports)]

use std::collections::VecDeque;
use std::fmt::Display;
use std::io::{self, prelude::*};

use mzdata::curie;
use mzdata::params::{CURIE, CURIEParsingError, ControlledVocabulary, ParamValue};
use mzpeaks::prelude::PeakCollectionMut;

use crate::model::{
    Analyte, AnnotatedPeak, IdType, Interpretation, InterpretationMember, LibraryHeader,
    LibrarySpectrum,
};

use crate::Term;
use crate::attr::{
    Attribute, AttributeParseError, AttributeSet, AttributeValue, AttributeValueParseError,
    Attributed, AttributedMut, EntryType, TermParserError,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParserState {
    Initial,
    Header,
    AttributeSet,
    Spectrum,
    Analyte,
    Interpretation,
    InterpretationMember,
    Peaks,
    Between,
}

impl Display for ParserState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, thiserror::Error)]
pub enum MzSpecLibTextParseError {
    #[error("An I/O error occured: {0}")]
    IOError(
        #[from]
        #[source]
        io::Error,
    ),
    #[error("Unexpected content {0} found in state {1}")]
    InvalidContentAtState(String, ParserState),
    #[error("Attribute parsing error {0} found in state {1}")]
    AttributeParseError(#[source] AttributeParseError, ParserState),
}

#[derive(Debug)]
pub struct MzSpecLibParser<R: Read> {
    inner: io::BufReader<R>,
    header: LibraryHeader,
    state: ParserState,
    line_cache: VecDeque<String>,
}

impl<R: Read> MzSpecLibParser<R> {
    pub fn new(reader: io::BufReader<R>) -> Result<Self, MzSpecLibTextParseError> {
        let mut this = Self {
            inner: reader,
            header: LibraryHeader::default(),
            state: ParserState::Initial,
            line_cache: VecDeque::new(),
        };
        this.read_header()?;
        Ok(this)
    }

    pub fn header(&self) -> &LibraryHeader {
        &self.header
    }

    fn push_back_line(&mut self, line: String) {
        self.line_cache.push_front(line);
    }

    fn read_next_line(&mut self, buf: &mut String) -> io::Result<usize> {
        buf.clear();
        if self.line_cache.is_empty() {
            self.inner.read_line(buf)
        } else {
            *buf = self.line_cache.pop_front().unwrap();
            Ok(buf.len())
        }
    }

    #[allow(unused)]
    fn peek_next_line(&mut self) -> io::Result<&str> {
        if self.line_cache.is_empty() {
            let mut buf = String::new();
            self.inner.read_line(&mut buf)?;
            self.line_cache.push_front(buf);
            Ok(self.line_cache.front().unwrap())
        } else {
            Ok(self.line_cache.front().unwrap())
        }
    }

    fn read_attribute(&mut self, buf: &mut String) -> Result<Attribute, MzSpecLibTextParseError> {
        self.read_next_line(buf)?;
        buf.trim_ascii_end()
            .parse()
            .map_err(|e| MzSpecLibTextParseError::AttributeParseError(e, self.state))
    }

    fn read_attribute_sets(&mut self) -> Result<(), MzSpecLibTextParseError> {
        let mut buf = String::new();
        let mut current_attribute_set: Option<AttributeSet> = None;

        loop {
            match self.read_attribute(&mut buf) {
                Ok(attr) => {
                    current_attribute_set.as_mut().unwrap().add_attribute(attr);
                }
                Err(e) => {
                    if buf.starts_with("<") {
                        if buf.starts_with("<Spectrum=") {
                            if current_attribute_set.is_some() {
                                let set = current_attribute_set.take().unwrap();
                                self.header
                                    .attribute_classes
                                    .entry(set.namespace)
                                    .or_default()
                                    .push(set);
                            }
                            self.state = ParserState::Spectrum;
                            self.push_back_line(buf);
                            break;
                        } else if buf.starts_with("<AttributeSet") {
                            self.state = ParserState::AttributeSet;
                            if let Some((_, rest)) = buf.trim().split_once(" ") {
                                if !rest.ends_with(">") {
                                    return Err(MzSpecLibTextParseError::InvalidContentAtState(
                                        buf, self.state,
                                    ));
                                }
                                if let Some((entry_tp, id)) = rest[..rest.len() - 1].split_once("=")
                                {
                                    if current_attribute_set.is_some() {
                                        let set = current_attribute_set.take().unwrap();
                                        self.header
                                            .attribute_classes
                                            .entry(set.namespace)
                                            .or_default()
                                            .push(set);
                                    }
                                    let set_entry_tp = match entry_tp {
                                        "Spectrum" => EntryType::Spectrum,
                                        "Analyte" => EntryType::Analyte,
                                        "Interpretation" => EntryType::Interpretation,
                                        "Cluster" => EntryType::Cluster,
                                        _ => {
                                            return Err(
                                                MzSpecLibTextParseError::InvalidContentAtState(
                                                    buf, self.state,
                                                ),
                                            );
                                        }
                                    };
                                    let set_id = id.to_string();
                                    current_attribute_set =
                                        Some(AttributeSet::new(set_id, set_entry_tp, Vec::new()));
                                } else {
                                    return Err(MzSpecLibTextParseError::InvalidContentAtState(
                                        buf, self.state,
                                    ));
                                }
                            } else {
                                return Err(MzSpecLibTextParseError::InvalidContentAtState(
                                    buf, self.state,
                                ));
                            }
                        } else {
                            return Err(MzSpecLibTextParseError::InvalidContentAtState(
                                buf, self.state,
                            ));
                        }
                    } else if buf.is_empty() {
                        continue;
                    } else {
                        return Err(e);
                    }
                }
            }
        }
        Ok(())
    }

    fn read_header(&mut self) -> Result<(), MzSpecLibTextParseError> {
        let mut buf = String::new();
        self.read_next_line(&mut buf)?;

        if !buf.starts_with("<mzSpecLib>") {
            return Err(MzSpecLibTextParseError::InvalidContentAtState(
                buf,
                ParserState::Initial,
            ));
        }
        self.state = ParserState::Header;

        loop {
            match self.read_attribute(&mut buf) {
                Ok(attr) => {
                    // TODO: switch to assigning these basic attributes to header fields
                    self.header.add_attribute(attr);
                }
                Err(e) => {
                    if buf.starts_with("<") {
                        if buf.starts_with("<Spectrum=") {
                            self.state = ParserState::Spectrum;
                        } else if buf.starts_with("<AttributeSet") {
                            self.state = ParserState::AttributeSet;
                        } else {
                            return Err(MzSpecLibTextParseError::InvalidContentAtState(
                                buf, self.state,
                            ));
                        }

                        self.push_back_line(buf);
                        break;
                    } else if buf.is_empty() {
                        continue;
                    } else {
                        return Err(e);
                    }
                }
            }
        }

        if !matches!(self.state, ParserState::AttributeSet) {
            return Ok(());
        }

        self.read_attribute_sets()?;

        Ok(())
    }

    fn parse_decl(
        &mut self,
        buf: &str,
        decl: &str,
        to_state: ParserState,
    ) -> Result<u32, MzSpecLibTextParseError> {
        let id = if buf.starts_with(decl) {
            self.state = to_state;
            let buf_ = buf.trim();
            if let Some((_, val)) = buf_[..buf_.len() - 1].split_once("=") {
                let id = val.parse::<IdType>().map_err(|_| {
                    MzSpecLibTextParseError::InvalidContentAtState(buf.to_string(), self.state)
                })?;
                id
            } else {
                return Err(MzSpecLibTextParseError::InvalidContentAtState(
                    buf.to_string(),
                    self.state,
                ));
            }
        } else {
            return Err(MzSpecLibTextParseError::InvalidContentAtState(
                buf.to_string(),
                self.state,
            ));
        };
        Ok(id)
    }

    fn read_analyte(&mut self) -> Result<Analyte, MzSpecLibTextParseError> {
        let mut buf = String::new();
        self.read_next_line(&mut buf)?;
        let id = self.parse_decl(&buf, "<Analyte=", ParserState::Analyte)?;

        let mut analyte = Analyte::new(id, Vec::new());
        loop {
            match self.read_attribute(&mut buf) {
                Ok(attr) => {
                    analyte.add_attribute(attr);
                }
                Err(e) => {
                    if buf.starts_with("<") {
                        self.push_back_line(buf);
                        break;
                    } else if buf.trim().is_empty() {
                        continue;
                    } else {
                        return Err(e);
                    }
                }
            }
        }
        Ok(analyte)
    }

    fn read_interpretation(&mut self) -> Result<Interpretation, MzSpecLibTextParseError> {
        let mut buf = String::new();
        self.read_next_line(&mut buf)?;
        let id = self.parse_decl(&buf, "<Interpretation=", ParserState::Interpretation)?;
        let mut interp = Interpretation::new(id, Vec::new(), Vec::new(), Vec::new());
        loop {
            match self.read_attribute(&mut buf) {
                Ok(attr) => {
                    interp.add_attribute(attr);
                }
                Err(e) => {
                    if buf.starts_with("<InterpretationMember=") {
                        self.push_back_line(buf);
                        todo!();
                    } else if buf.starts_with("<") {
                        self.push_back_line(buf);
                        break;
                    } else if buf.trim().is_empty() {
                        continue;
                    } else {
                        return Err(e);
                    }
                }
            }
        }
        Ok(interp)
    }

    fn parse_peak_line(&self, buf: &str) -> Result<AnnotatedPeak, MzSpecLibTextParseError> {
        let buf = buf.trim();
        let mut it = buf.split("\t");

        let mz = match it.next().and_then(|v| v.parse::<f64>().ok()) {
            Some(v) => v,
            None => {
                return Err(MzSpecLibTextParseError::InvalidContentAtState(
                    buf.to_string(),
                    ParserState::Peaks,
                ));
            }
        };

        let intensity = match it.next().and_then(|v| v.parse::<f32>().ok()) {
            Some(v) => v,
            None => {
                return Err(MzSpecLibTextParseError::InvalidContentAtState(
                    buf.to_string(),
                    ParserState::Peaks,
                ));
            }
        };

        let mut peak = AnnotatedPeak::new(mz, intensity, 0, String::new(), String::new());

        match it.next() {
            Some(v) => {
                peak.annotations = v.to_string();
            },
            None => return Ok(peak)
        }

        match it.next() {
            Some(v) => {
                peak.aggregations = v.to_string();
            },
            None => return Ok(peak)
        }

        Ok(peak)
    }

    fn read_spectrum(&mut self) -> Result<LibrarySpectrum, MzSpecLibTextParseError> {
        let mut buf = String::new();

        self.read_next_line(&mut buf)?;
        let id = self.parse_decl(&buf, "<Spectrum=", ParserState::Spectrum)?;
        let mut spec = LibrarySpectrum::new(
            id,
            0,
            Box::default(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Default::default(),
        );

        loop {
            match self.read_attribute(&mut buf) {
                Ok(attr) => match attr.name.accession {
                    CURIE {
                        controlled_vocabulary: ControlledVocabulary::MS,
                        accession: 1003061,
                    } => spec.name = attr.value.to_string().into_boxed_str(),
                    CURIE {
                        controlled_vocabulary: ControlledVocabulary::MS,
                        accession: 1003062,
                    } => {
                        spec.index = attr.value.scalar().to_u64().map_err(|v| {
                            MzSpecLibTextParseError::AttributeParseError(
                                AttributeParseError::ValueParseError(
                                    AttributeValueParseError::ParamValueParseError(v),
                                    buf.clone(),
                                ),
                                self.state,
                            )
                        })? as usize
                    }
                    _ => spec.add_attribute(attr),
                },
                Err(e) => {
                    if buf.starts_with("<Analyte=") {
                        self.push_back_line(buf.clone());
                        let analyte = self.read_analyte()?;
                        spec.analytes.push(analyte);
                    } else if buf.starts_with("<Interpretation=") {
                        self.push_back_line(buf.clone());
                        let interp = self.read_interpretation()?;
                        spec.interpretations.push(interp);
                    } else if buf.starts_with("<Peaks>") {
                        self.state = ParserState::Peaks;
                        break;
                    } else if buf.trim().is_empty() {
                        continue;
                    } else {
                        return Err(e);
                    }
                }
            }
        }

        if matches!(self.state, ParserState::Peaks) {
            loop {
                self.read_next_line(&mut buf)?;
                let buf_trimmed = buf.trim();
                if buf_trimmed.starts_with("<") {
                    self.push_back_line(buf);
                    self.state = ParserState::Between;
                    break;
                } else if buf_trimmed.is_empty() {
                    self.state = ParserState::Between;
                    break;
                }

                let peak = self.parse_peak_line(&buf_trimmed)?;
                spec.peaks.push(peak);
            }
        }

        Ok(spec)
    }

    pub fn read_next(&mut self) -> Result<LibrarySpectrum, MzSpecLibTextParseError> {
        self.read_spectrum()
    }

    pub fn inner(&self) -> &io::BufReader<R> {
        &self.inner
    }

    pub fn into_inner(self) -> io::BufReader<R> {
        self.inner
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use super::*;

    #[test]
    fn test_header() -> Result<(), MzSpecLibTextParseError> {
        let buf = io::BufReader::new(fs::File::open(
            "test/data/chinese_hamster_hcd_selected_head.mzspeclib.txt",
        )?);

        let mut this = MzSpecLibParser::new(buf)?;
        let header = this.header();
        eprintln!("{header:?}");
        // TODO: switch to assigning these basic attributes to header fields
        assert_eq!(header.attributes().len(), 2);

        let spec = this.read_next()?;
        eprintln!("{spec}");

        Ok(())
    }
}
