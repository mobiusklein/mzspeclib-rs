use std::fmt::{Display, Write};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PeptideBackboneIonSeries {
    A,
    B,
    C,
    D,
    W,
    X,
    Y,
    Z,
}

impl Display for PeptideBackboneIonSeries {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PeptideBackboneIonSeries::A => write!(f, "a"),
            PeptideBackboneIonSeries::B => write!(f, "b"),
            PeptideBackboneIonSeries::C => write!(f, "c"),
            PeptideBackboneIonSeries::D => write!(f, "d"),
            PeptideBackboneIonSeries::W => write!(f, "w"),
            PeptideBackboneIonSeries::X => write!(f, "x"),
            PeptideBackboneIonSeries::Y => write!(f, "y"),
            PeptideBackboneIonSeries::Z => write!(f, "z"),
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub enum IonType {
    ImmoniumFragment {
        amino_acid: char,
        modification: Option<Box<str>>,
    },
    PeptideBackboneFragment {
        series: PeptideBackboneIonSeries,
        position: usize,
        sequence: Option<Box<str>>,
    },
    InternalFragment {
        start: usize,
        end: usize,
        sequence: Option<Box<str>>,
    },
    Precursor,
    ReferenceMolecule(Box<str>),
    NamedCompound(Box<str>),
    Formula(Vec<FormulaComponent>),
    SMILES(Box<str>),
    #[default]
    Unannotated,
}

impl Display for IonType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IonType::ImmoniumFragment { amino_acid, modification } => {
                write!(f, "I{amino_acid}")?;
                if let Some(modification) = modification {
                    write!(f, "{modification}")?;
                }
                Ok(())
            },
            IonType::PeptideBackboneFragment { series, position, sequence } => {
                write!(f, "{series}{position}")?;
                if let Some(seq) = sequence {
                    write!(f, "{{{seq}}}")?;
                }
                Ok(())
            },
            IonType::InternalFragment { start, end, sequence } => {
                write!(f, "m{start}:{end}")?;
                if let Some(seq) = sequence {
                    write!(f, "{{{seq}}}")?;
                }
                Ok(())
            },
            IonType::Precursor => write!(f, "p"),
            IonType::ReferenceMolecule(r) => {
                write!(f, "r{r}")
            },
            IonType::NamedCompound(c) => {
                write!(f, "_{{{c}}}")
            },
            IonType::Formula(formula_components) => {
                write!(f, "f{{")?;
                for c in formula_components.iter() {
                    write!(f, "{c}")?;
                }
                f.write_str("}")
            },
            IonType::SMILES(s) => write!(f, "s{{{s}}}"),
            IonType::Unannotated => f.write_str("?"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Tolerance {
    Da(f64),
    PPM(f64),
}

impl Display for Tolerance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tolerance::Da(v) => write!(f, "{v}"),
            Tolerance::PPM(v) => write!(f, "{v}ppm"),
        }
    }
}

impl Default for Tolerance {
    fn default() -> Self {
        Self::Da(0.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum Isotope {
    Basic { coefficient: i32 },
    Element { coefficient: i32, nucleon_count: i32, element: Box<str> },
    Averaged { coefficient: i32 },
}

impl Display for Isotope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Isotope::Basic { coefficient } => {
                write!(f, "{}", if *coefficient > 0 {'+'} else {'-'})?;
                if coefficient.abs() > 1 {
                    write!(f, "{}", coefficient.abs())?;
                }
                f.write_char('i')
            },
            Isotope::Element { coefficient, nucleon_count, element } => {
                write!(f, "{}", if *coefficient > 0 {'+'} else {'-'})?;
                if coefficient.abs() > 1 {
                    write!(f, "{}", coefficient.abs())?;
                }
                f.write_str("i")?;
                write!(f, "{nucleon_count}")?;
                write!(f, "{element}")
            },
            Isotope::Averaged { coefficient } => {
                write!(f, "{}", if *coefficient > 0 {'+'} else {'-'})?;
                if coefficient.abs() > 1 {
                    write!(f, "{}", coefficient.abs())?;
                }
                f.write_str("iA")
            },
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FormulaComponent {
    pub isotope: Option<i32>,
    pub element: Box<str>,
    pub count: i32,
}

impl Display for FormulaComponent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(iso) = self.isotope {
            write!(f, "[{iso}]")?;
        }
        write!(f, "{}{}", self.element, self.count)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum NeutralLoss {
    Formula { coefficient: i32, formula: Vec<FormulaComponent> },
    ReferenceMolecule { coefficient: i32, name: Box<str> },
}

impl Display for NeutralLoss {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NeutralLoss::Formula { coefficient, formula } => {
                write!(f, "{}", if *coefficient > 0 {'+'} else {'-'})?;
                if coefficient.abs() > 1 {
                    write!(f, "{}", coefficient.abs())?;
                }
                for c in formula.iter() {
                    write!(f, "{c}")?
                }
                Ok(())
            },
            NeutralLoss::ReferenceMolecule { coefficient, name } => {
                write!(f, "{}", if *coefficient > 0 {'+'} else {'-'})?;
                if coefficient.abs() > 1 {
                    write!(f, "{}", coefficient.abs())?;
                }
                write!(f, "{name}")?;
                Ok(())
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SignedFormulaComponent {
    pub coefficient: i32,
    pub component: FormulaComponent
}

impl Display for SignedFormulaComponent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", if self.coefficient > 0 {'+'} else {'-'})?;
        if self.coefficient.abs() > 1 {
            write!(f, "{}", self.coefficient.abs())?;
        }
        write!(f, "{}", self.component)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub struct Adduct(pub Vec<SignedFormulaComponent>);

impl std::ops::Deref for Adduct {
    type Target = Vec<SignedFormulaComponent>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for Adduct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[M")?;
        for c in self.0.iter() {
            write!(f, "{c}")?;
        }
        Ok(())
    }
}

/// An mzPAF single peak annotation.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct PeakAnnotation {
    pub auxiliary: bool,
    pub analyte_number: usize,
    pub ion: IonType,
    pub neutral_losses: Vec<NeutralLoss>,
    pub isotopes: Vec<Isotope>,
    pub charge: i32,
    pub adduct: Option<Adduct>,
    pub deviation: Option<Tolerance>,
    pub confidence: Option<f64>,
}

impl Display for PeakAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.auxiliary {
            f.write_char('&')?
        }
        if !matches!(self.ion, IonType::Unannotated) {
            write!(f, "{}@", self.analyte_number)?;
        }
        write!(f, "{}", self.ion)?;
        for nl in self.neutral_losses.iter() {
            write!(f, "{nl}")?;
        }
        for i in self.isotopes.iter() {
            write!(f, "{i}")?;
        }
        if self.charge.abs() > 1 {
            write!(f, "^{}", self.charge.abs())?;
        }
        if let Some(adduct) = self.adduct.as_ref() {
            write!(f, "{adduct}")?;
        }
        if let Some(dev) = self.deviation.as_ref() {
            write!(f, "/{dev}")?;
        }
        if let Some(conf) = self.confidence.as_ref() {
            write!(f, "*{conf}")?;
        }
        Ok(())
    }
}

impl PeakAnnotation {
    pub fn new(
        auxiliary: bool,
        analyte_number: usize,
        ion: IonType,
        neutral_losses: Vec<NeutralLoss>,
        isotopes: Vec<Isotope>,
        adduct: Option<Adduct>,
        charge: i32,
        deviation: Option<Tolerance>,
        confidence: Option<f64>,
    ) -> Self {
        Self {
            auxiliary,
            analyte_number,
            ion,
            neutral_losses,
            isotopes,
            adduct,
            charge,
            deviation,
            confidence,
        }
    }
}
