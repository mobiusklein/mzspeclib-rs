use std::{borrow::Cow, fmt::Display, str::FromStr};

use mzdata::{
    curie,
    params::{CURIE, CURIEParsingError, ParamValue, ParamValueParseError, Value},
};

#[derive(Debug, thiserror::Error)]
pub enum TermParserError {
    #[error("Malformed CURIE: {0}")]
    CURIEError(
        #[from]
        #[source]
        CURIEParsingError,
    ),
    #[error("The pipe character is missing from term expression {0}")]
    MissingPipe(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Term {
    pub accession: CURIE,
    pub name: Box<str>,
}

impl FromStr for Term {
    type Err = TermParserError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some((curie, name)) = s.split_once("|") {
            let curie: CURIE = curie.parse()?;
            let name = name.to_string().into_boxed_str();
            Ok(Term::new(curie, name))
        } else {
            Err(TermParserError::MissingPipe(s.to_string()))
        }
    }
}

impl Term {
    pub fn new(accession: CURIE, name: Box<str>) -> Self {
        Self { accession, name }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}|{}", self.accession, self.name)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum AttributeValueParseError {
    #[error("{0}")]
    ParamValueParseError(
        #[from]
        #[source]
        ParamValueParseError,
    ),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AttributeValue {
    Scalar(Value),
    List(Vec<Value>),
    Term(Term),
}

impl AttributeValue {
    pub fn scalar(&self) -> Cow<'_, Value> {
        match self {
            AttributeValue::Scalar(value) => Cow::Borrowed(value),
            AttributeValue::List(_) => Cow::Owned(Value::String(self.to_string())),
            AttributeValue::Term(term) => Cow::Owned(Value::String(term.to_string())),
        }
    }
}

impl FromStr for AttributeValue {
    type Err = AttributeValueParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(term) = s.parse() {
            Ok(Self::Term(term))
        } else if s.contains(",") {
            let vals: Vec<Value> = s.split(",").flat_map(Value::from_str).collect();
            if vals.iter().all(|v| v.is_numeric() || v.is_boolean()) {
                Ok(Self::List(vals))
            } else {
                let v = Value::from_str(s)?;
                Ok(Self::Scalar(v))
            }
        } else {
            let v = Value::from_str(s)?;
            Ok(Self::Scalar(v))
        }
    }
}

impl From<Value> for AttributeValue {
    fn from(value: Value) -> Self {
        Self::Scalar(value)
    }
}

impl From<Term> for AttributeValue {
    fn from(value: Term) -> Self {
        Self::Term(value)
    }
}

impl Display for AttributeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttributeValue::Scalar(value) => write!(f, "{}", value),
            AttributeValue::List(values) => {
                for (i, val) in values.iter().enumerate() {
                    write!(f, "{}", val)?;
                    if i < values.len() - 1 {
                        f.write_str(",")?;
                    }
                }
                Ok(())
            }
            AttributeValue::Term(attribute_name) => {
                f.write_str(attribute_name.to_string().as_str())
            }
        }
    }
}

impl From<Vec<Value>> for AttributeValue {
    fn from(value: Vec<Value>) -> Self {
        Self::List(value)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum AttributeParseError {
    #[error("Failed to parse the term component of {1}: {0}")]
    TermParserError(#[source] TermParserError, String),
    #[error("Failed to parse the value component of {1}: {0}")]
    ValueParseError(#[source] AttributeValueParseError, String),
    #[error("Failed to parse the group id component of {1}: {0}")]
    GroupIDParseError(#[source] std::num::ParseIntError, String),
    #[error("The '=' character was not found separating the term and the value in {0}")]
    MissingValueSeparator(String),
    #[error("The attribute line {0} is malformed")]
    Malformed(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: Term,
    pub value: AttributeValue,
    pub group_id: Option<u32>,
}

impl FromStr for Attribute {
    type Err = AttributeParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("[") {
            if let Some((group_id, rest)) = s[1..].split_once("]") {
                let group_id = match group_id.parse::<u32>() {
                    Ok(group_id) => group_id,
                    Err(e) => return Err(AttributeParseError::GroupIDParseError(e, s.to_string())),
                };
                if let Some((name, val)) = rest.split_once("=") {
                    let term: Term = name
                        .parse()
                        .map_err(|e| AttributeParseError::TermParserError(e, s.to_string()))?;
                    let value: AttributeValue = val
                        .parse()
                        .map_err(|e| AttributeParseError::ValueParseError(e, s.to_string()))?;
                    Ok(Attribute::new(term, value, Some(group_id)))
                } else {
                    Err(AttributeParseError::MissingValueSeparator(s.to_string()))
                }
            } else {
                Err(Self::Err::Malformed(s.to_string()))
            }
        } else {
            if let Some((name, val)) = s.split_once("=") {
                let term: Term = name
                    .parse()
                    .map_err(|e| AttributeParseError::TermParserError(e, s.to_string()))?;
                let value: AttributeValue = val
                    .parse()
                    .map_err(|e| AttributeParseError::ValueParseError(e, s.to_string()))?;
                Ok(Attribute::new(term, value, None))
            } else {
                Err(AttributeParseError::MissingValueSeparator(s.to_string()))
            }
        }
    }
}

impl Attribute {
    pub fn new(name: Term, value: AttributeValue, group_id: Option<u32>) -> Self {
        Self {
            name,
            value,
            group_id,
        }
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.group_id {
            Some(i) => {
                write!(f, "[{i}]{}={}", self.name, self.value)
            }
            None => {
                write!(f, "{}={}", self.name, self.value)
            }
        }
    }
}

pub trait Attributed {
    fn attributes(&self) -> &[Attribute];

    fn find(&self, term: &Term) -> Option<(usize, &Attribute)> {
        if let Some(i) = self.attributes().iter().position(|v| {
            if v.name == *term {
                true
            } else if v.name.accession == curie!(MS:1003275) {
                if let AttributeValue::Term(v) = &v.value {
                    *v == *term
                } else {
                    false
                }
            } else {
                false
            }
        }) {
            Some((i, &self.attributes()[i]))
        } else {
            None
        }
    }

    fn find_group(&self, group_id: u32) -> Vec<&Attribute> {
        self.attributes()
            .iter()
            .filter(|a| a.group_id == Some(group_id))
            .collect()
    }

    fn find_all(&self, term: &Term) -> impl Iterator<Item = &Attribute> {
        self.attributes().iter().filter(|v| {
            if v.name == *term {
                true
            } else if v.name.accession == curie!(MS:1003275) {
                if let AttributeValue::Term(v) = &v.value {
                    *v == *term
                } else {
                    false
                }
            } else {
                false
            }
        })
    }
}

pub trait AttributedMut: Attributed {
    fn attributes_mut(&mut self) -> &mut [Attribute];

    fn find_group_mut(&mut self, group_id: u32) -> impl Iterator<Item = &mut Attribute> {
        self.attributes_mut()
            .iter_mut()
            .filter(move |a| a.group_id == Some(group_id))
    }

    fn add_attribute(&mut self, attr: Attribute);

    fn remove_attribute(&mut self, index: usize) -> Attribute;

    fn extend_attributes(&mut self, attrs: impl IntoIterator<Item = Attribute>) {
        for attr in attrs {
            self.add_attribute(attr);
        }
    }
}

macro_rules! impl_attributed {
    ($t:ty) => {
        impl Attributed for $t {
            fn attributes(&self) -> &[Attribute] {
                &self.attributes
            }
        }
    };

    (mut $t:ty) => {
        impl_attributed!($t);

        impl AttributedMut for $t {
            fn attributes_mut(&mut self) -> &mut [Attribute] {
                &mut self.attributes
            }

            fn add_attribute(&mut self, attr: Attribute) {
                self.attributes.push(attr);
            }

            fn remove_attribute(&mut self, index: usize) -> Attribute {
                self.attributes.remove(index)
            }
        }
    };
}

pub(crate) use impl_attributed;

impl Attributed for Vec<Attribute> {
    fn attributes(&self) -> &[Attribute] {
        &self
    }
}

impl AttributedMut for Vec<Attribute> {
    fn attributes_mut(&mut self) -> &mut [Attribute] {
        self
    }

    fn add_attribute(&mut self, attr: Attribute) {
        self.push(attr);
    }

    fn remove_attribute(&mut self, index: usize) -> Attribute {
        self.remove(index)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AttributeSet {
    pub id: String,
    pub namespace: EntryType,
    pub attributes: Vec<Attribute>,
}

impl_attributed!(mut AttributeSet);

impl AttributeSet {
    pub fn new(id: String, namespace: EntryType, attributes: Vec<Attribute>) -> Self {
        Self { id, namespace, attributes }
    }
}

impl PartialOrd for AttributeSet {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.id.partial_cmp(&other.id) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.namespace.partial_cmp(&other.namespace) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.attributes.len().partial_cmp(&other.attributes.len())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EntryType {
    Spectrum,
    Analyte,
    Interpretation,
    Cluster,
}
