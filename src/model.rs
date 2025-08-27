use std::collections::HashMap;
use std::fmt::Display;

use mzdata::curie;
use mzdata::params::Value;

use mzpeaks::{MZPeakSetType, prelude::*};

use crate::attr::{Attribute, AttributeValue, Attributed, AttributedMut, Term, impl_attributed};
use crate::{AttributeSet, EntryType};

#[derive(Debug, Clone, Default)]
pub struct LibraryHeader {
    pub format_version: String,
    pub attributes: Vec<Attribute>,
    pub attribute_classes: HashMap<EntryType, Vec<AttributeSet>>,
}

impl_attributed!(mut LibraryHeader);

impl Display for LibraryHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let version = Attribute::new(
            Term::new(curie!(MS:1003186), "library format version".into()),
            AttributeValue::Scalar(Value::String(self.format_version.clone())),
            None,
        );
        writeln!(f, "<MzSpecLib>\n{version}")?;
        for attr in self.attributes.iter() {
            writeln!(f, "{attr}")?;
        }
        Ok(())
    }
}

impl LibraryHeader {
    pub fn new(
        format_version: String,
        attributes: Vec<Attribute>,
        attribute_classes: HashMap<EntryType, Vec<AttributeSet>>,
    ) -> Self {
        Self {
            format_version,
            attributes,
            attribute_classes,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct AnnotatedPeak {
    pub mz: f64,
    pub intensity: f32,
    pub index: mzpeaks::IndexType,
    pub annotations: String,
    pub aggregations: String,
}

impl AnnotatedPeak {
    pub fn new(mz: f64, intensity: f32, index: mzpeaks::IndexType, annotations: String, aggregations: String) -> Self {
        Self { mz, intensity, index, annotations, aggregations }
    }
}

mzpeaks::implement_centroidlike!(AnnotatedPeak, true);

impl Display for AnnotatedPeak {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\t{}", self.mz, self.intensity)?;

        if !self.annotations.is_empty() {
            f.write_str("\t")?;
            write!(f, "{}", self.annotations)?;
            // let n = self.annotations.len();
            // for (i, annot) in self.annotations.iter().enumerate() {
            //     write!(f, "{annot}")?;
            //     if i < n - 1 {
            //         f.write_str(",")?;
            //     }
            // }
        }
        if !self.aggregations.is_empty() {
            f.write_str("\t")?;
            write!(f, "{}", self.aggregations)?;
            // let n = self.aggregations.len();
            // for (i, aggr) in self.aggregations.iter().enumerate() {
            //     write!(f, "{aggr}")?;
            //     if i < n - 1 {
            //         f.write_str(",")?;
            //     }
            // }
        }
        Ok(())
    }
}

pub type IdType = u32;

#[derive(Default, Debug, Clone)]
pub struct Analyte {
    pub id: IdType,
    pub attributes: Vec<Attribute>,
}

impl_attributed!(mut Analyte);

impl Display for Analyte {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "<Analyte={}>", self.id)?;
        for attr in self.attributes.iter() {
            writeln!(f, "{attr}")?;
        }
        Ok(())
    }
}

impl Analyte {
    pub fn new(id: IdType, attributes: Vec<Attribute>) -> Self {
        Self { id, attributes }
    }
}

#[derive(Default, Debug, Clone)]
pub struct Interpretation {
    pub id: IdType,
    pub attributes: Vec<Attribute>,
    pub analyte_refs: Vec<IdType>,
    pub members: Vec<InterpretationMember>,
}

impl Interpretation {
    pub fn new(id: IdType, attributes: Vec<Attribute>, analyte_refs: Vec<IdType>, members: Vec<InterpretationMember>) -> Self {
        Self { id, attributes, analyte_refs, members }
    }
}

impl_attributed!(mut Interpretation);

impl Display for Interpretation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "<Interpretation={}>", self.id)?;
        if !self.analyte_refs.is_empty() {
            let val = AttributeValue::List(
                self.analyte_refs
                    .iter()
                    .map(|v| Value::Int((*v) as i64))
                    .collect(),
            );
            let mixture_ids = Attribute::new(
                Term::new(curie!(MS:1003163), "analyte mixture members".into()),
                val,
                None,
            );
            writeln!(f, "{mixture_ids}")?;
        }
        for attr in self.attributes.iter() {
            writeln!(f, "{attr}")?;
        }
        for member in self.members.iter() {
            write!(f, "{member}")?;
        }
        Ok(())
    }
}

#[derive(Default, Debug, Clone)]
pub struct InterpretationMember {
    pub id: IdType,
    pub attributes: Vec<Attribute>,
    pub analyte_ref: IdType,
}

impl_attributed!(mut InterpretationMember);

impl Display for InterpretationMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "<InterpretationMember={}>", self.id)?;
        for attr in self.attributes.iter() {
            f.write_str(attr.to_string().as_str())?;
        }
        Ok(())
    }
}

#[derive(Default, Debug, Clone)]
pub struct LibrarySpectrum {
    pub key: IdType,
    pub index: usize,
    pub name: Box<str>,
    pub attributes: Vec<Attribute>,
    pub analytes: Vec<Analyte>,
    pub interpretations: Vec<Interpretation>,
    pub peaks: MZPeakSetType<AnnotatedPeak>,
}

impl_attributed!(mut LibrarySpectrum);

impl LibrarySpectrum {
    pub fn new(
        key: IdType,
        index: usize,
        name: Box<str>,
        attributes: Vec<Attribute>,
        analytes: Vec<Analyte>,
        interpretations: Vec<Interpretation>,
        peaks: MZPeakSetType<AnnotatedPeak>,
    ) -> Self {
        Self {
            key,
            index,
            name,
            attributes,
            analytes,
            interpretations,
            peaks,
        }
    }
}

impl Display for LibrarySpectrum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "<Spectrum={}>", self.key)?;
        writeln!(f, "MS:1003061|library spectrum name={}", self.name)?;
        for attr in self.attributes() {
            writeln!(f, "{attr}")?;
        }
        for analyte in self.analytes.iter() {
            write!(f, "{analyte}")?;
        }
        for interp in self.interpretations.iter() {
            write!(f, "{interp}")?;
        }
        writeln!(f, "<Peaks>")?;
        for p in self.peaks.iter() {
            writeln!(f, "{p}")?;
        }
        Ok(())
    }
}