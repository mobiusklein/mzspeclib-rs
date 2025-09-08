use super::{
    Adduct, FormulaComponent, IonType, Isotope, NeutralLoss, PeakAnnotation,
    PeptideBackboneIonSeries, SignedFormulaComponent, Tolerance,
};

use nom;
use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::combinator::{all_consuming, cond, opt};
use nom::error::{context, ParseError};
use nom_language::error::{VerboseError, VerboseErrorKind};
use nom::multi::{many0, many1, separated_list0};
use nom::number::complete::recognize_float_or_exceptions;
use nom::{
    Parser,
    character::complete::{char, i32 as parse_i32, one_of, usize as parse_usize},
    sequence::terminated,
};

pub type IResult<I, O> = nom::IResult<I, O, VerboseError<I>>;

fn read_analyte_reference(line: &str) -> IResult<&str, Option<usize>> {
    if let Ok((line, digits)) =
        terminated(parse_usize::<&str, nom::error::Error<&str>>, char('@')).parse(line)
    {
        Ok((line, Some(digits)))
    } else {
        Ok((line, None))
    }
}

fn read_auxiliary(line: &str) -> IResult<&str, bool> {
    let (is_auxiliary, line) =
        if let Ok((line, _tok)) = char::<&str, nom::error::Error<&str>>('&')(line) {
            (true, line)
        } else {
            (false, line)
        };
    IResult::Ok((line, is_auxiliary))
}

const fn nested(
    opening_bracket: char,
    closing_bracket: char,
) -> impl Fn(&str) -> IResult<&str, &str> {
    move |i: &str| -> IResult<&str, &str> {
        let mut index = 0;
        let mut bracket_counter = 0;
        if !i.starts_with(opening_bracket) {
            return Err(nom::Err::Error(
                VerboseError {
                    errors: vec![(i, VerboseErrorKind::Nom(nom::error::ErrorKind::Not))]
                }
            ))
        }
        while let Some(n) = &i[index..].find(&[opening_bracket, closing_bracket, '\\'][..]) {
            index += n;
            let mut it = i[index..].chars();
            match it.next() {
                Some('\\') => {
                    // Skip the escape char `\`.
                    index += '\\'.len_utf8();
                    // Skip also the following char.
                    if let Some(c) = it.next() {
                        index += c.len_utf8();
                    }
                }
                Some(c) if c == opening_bracket => {
                    bracket_counter += 1;
                    index += opening_bracket.len_utf8();
                }
                Some(c) if c == closing_bracket => {
                    // Closing bracket.
                    bracket_counter -= 1;
                    index += closing_bracket.len_utf8();
                }
                // Can not happen.
                _ => unreachable!(),
            };
            // We found the unmatched closing bracket.
            if bracket_counter == 0 {
                // We do not consume it.
                // index += closing_bracket.len_utf8();
                return Ok((&i[index..], &i[0..index]));
            };
        }
        if bracket_counter == 0 {
            Ok(("", i))
        } else {
            return Err(nom::Err::Error(
                VerboseError {
                    errors: vec![(i, VerboseErrorKind::Nom(nom::error::ErrorKind::TakeUntil))]
                }
            ))
        }
    }
}

fn read_backbone_ion(line: &str) -> IResult<&str, IonType> {
    let (line, series) = one_of("abcdwxyz")(line)?;
    let series = match series {
        'a' => PeptideBackboneIonSeries::A,
        'b' => PeptideBackboneIonSeries::B,
        'c' => PeptideBackboneIonSeries::C,
        'd' => PeptideBackboneIonSeries::D,
        'w' => PeptideBackboneIonSeries::W,
        'x' => PeptideBackboneIonSeries::X,
        'y' => PeptideBackboneIonSeries::Y,
        'z' => PeptideBackboneIonSeries::Z,
        _ => panic!("not possible"),
    };
    let (line, position) = parse_usize(line)?;
    let (line, sequence) = if line.starts_with('{') {
        let (line, seq) = nested('{', '}')(line)?;
        let seq: Box<str> = seq.chars().collect();
        (line, Some(seq))
    } else {
        (line, None)
    };
    Ok((
        line,
        IonType::PeptideBackboneFragment {
            series: series,
            position,
            sequence,
        },
    ))
}

fn read_internal_ion(line: &str) -> IResult<&str, IonType> {
    let (line, _) = char('m')(line)?;
    let (line, start) = parse_usize(line)?;
    let (line, _) = char(':')(line)?;
    let (line, end) = parse_usize(line)?;
    let (line, sequence) = if line.starts_with('{') {
        let (line, seq) = nested('{', '}')(line)?;
        let seq: Box<str> = seq.chars().collect();
        (line, Some(seq))
    } else {
        (line, None)
    };
    Ok((
        line,
        IonType::InternalFragment {
            start,
            end,
            sequence,
        },
    ))
}

fn read_immonium(line: &str) -> IResult<&str, IonType> {
    let (line, _) = context("Recognizing the series identifier", char('I')).parse(line)?;
    let (line, aa) = context(
        "Recognizing immonium amino acid",
        one_of("QWERTYUIOPASDFGHJKLMNBVCXZ"),
    )
    .parse(line)?;
    if line.starts_with("[") {
        let (line, rest) = context("Extracting the modification", nested('[', ']')).parse(line)?;
        let ion = IonType::ImmoniumFragment {
            amino_acid: aa,
            modification: Some(rest[1..rest.len() - 1].to_string().into_boxed_str()),
        };
        Ok((line, ion))
    } else {
        let ion = IonType::ImmoniumFragment {
            amino_acid: aa,
            modification: None,
        };
        Ok((line, ion))
    }
}

fn read_precursor(line: &str) -> IResult<&str, IonType> {
    let (line, _) = char('p')(line)?;
    Ok((line, IonType::Precursor))
}

fn read_reference_ion(line: &str) -> IResult<&str, IonType> {
    let (line, _) = char('r')(line)?;
    let (line, content) = nested('[', ']')(line)?;
    Ok((
        line,
        IonType::ReferenceMolecule(content.to_string().into_boxed_str()),
    ))
}

fn read_formula_ion_type(line: &str) -> IResult<&str, IonType> {
    let (line, _) = char('f')(line)?;
    let (line, content) = nested('{', '}')(line)?;
    let (_, formula) = all_consuming(cond(content.len() > 2, parse_formula))
        .parse(&content[1..content.len() - 1])?;
    let formula = formula.unwrap_or_default();
    Ok((line, IonType::Formula(formula)))
}

fn read_named_ion_type(line: &str) -> IResult<&str, IonType> {
    let (line, _) = char('_')(line)?;
    let (line, content) = nested('{', '}')(line)?;
    let content: Box<str> = if content.is_empty() {
        Box::default()
    } else {
        content[1..content.len() - 1].chars().collect()
    };
    Ok((line, IonType::NamedCompound(content)))
}

fn read_smiles_ion_type(line: &str) -> IResult<&str, IonType> {
    let (line, _) = char('s')(line)?;
    let (line, content) = nested('{', '}')(line)?;
    let content: Box<str> = if content.is_empty() {
        Box::default()
    } else {
        content[1..content.len() - 1].chars().collect()
    };
    Ok((line, IonType::SMILES(content)))
}

fn read_unannoted_ion(line: &str) -> IResult<&str, IonType> {
    let (line, _) = char('?')(line)?;
    Ok((line, IonType::Unannotated))
}

fn read_ion_type(line: &str) -> IResult<&str, IonType> {
    let (line, ion) = if let Some(c) = line.chars().next() {
        match c {
            '?' => read_unannoted_ion(line),
            'm' => read_internal_ion(line),
            'I' => read_immonium(line),
            'p' => read_precursor(line),
            'r' => read_reference_ion(line),
            'f' => read_formula_ion_type(line),
            '_' => read_named_ion_type(line),
            's' => read_smiles_ion_type(line),
            _ => read_backbone_ion(line)
        }
    } else {
        let e = VerboseError::from_error_kind(line, nom::error::ErrorKind::NonEmpty);
        return Err(nom::Err::Failure(e))
    }?;
    Ok((line, ion))
}

fn parse_element_name(line: &str) -> IResult<&str, (Option<i32>, Box<str>)> {
    let (line, isotope) = opt(nested('[', ']')).parse(line).unwrap();
    let isotope = if let Some(iso) = isotope {
        let (_, iso) = nom::combinator::all_consuming(nom::character::complete::i32).parse(iso)?;
        Some(iso)
    } else {
        None
    };
    let (line, e1) = one_of::<_, &str, _>("QWERTYUIOPASDFGHKLZXCVBNM").parse(line)?;
    let (line, e2) = opt(one_of::<_, &str, _>("qwertyuiopasdfghklzxcvbnm")).parse(line)?;
    let (line, element) = if let Some(e2) = e2 {
        let (line, e3) = opt(one_of::<_, &str, _>("qwertyuiopasdfghklzxcvbnm")).parse(line)?;
        let elt = if let Some(e3) = e3 {
            format!("{e1}{e2}{e3}").into_boxed_str()
        } else {
            format!("{e1}{e2}").into_boxed_str()
        };
        (line, elt)
    } else {
        let elt = e1.to_string().into_boxed_str();
        (line, elt)
    };

    Ok((line, (isotope, element)))
}

fn parse_element(line: &str) -> IResult<&str, FormulaComponent> {
    let (line, (isotope, element)) = context("element name", parse_element_name).parse(line)?;

    let (line, cnt) = opt(parse_i32).parse(line)?;
    let count = cnt.unwrap_or(1);

    let this = FormulaComponent {
        isotope,
        element,
        count,
    };

    Ok((line, this))
}

fn parse_formula(line: &str) -> IResult<&str, Vec<FormulaComponent>> {
    many1(parse_element).parse(line)
}

fn parse_signed_formula_component(line: &str) -> IResult<&str, SignedFormulaComponent> {
    let (line, coefficient) = opt(parse_signed).parse(line)?;
    let (line, elt) = parse_element(line)?;
    Ok((
        line,
        SignedFormulaComponent {
            coefficient: coefficient.unwrap_or(1),
            component: elt,
        },
    ))
}

fn read_neutral_loss(line: &str) -> IResult<&str, NeutralLoss> {
    let (line, coef) = parse_signed(line)?;
    if line.starts_with('[') {
        let (line, refname) =
            context("neutral loss reference name", nested('[', ']')).parse(line)?;
        Ok((
            line,
            NeutralLoss::ReferenceMolecule {
                coefficient: coef,
                name: refname.to_string().into_boxed_str(),
            },
        ))
    } else {
        let (line, formula) = context("neutral loss formula", parse_formula).parse(line)?;
        Ok((
            line,
            NeutralLoss::Formula {
                coefficient: coef,
                formula,
            },
        ))
    }
}

fn read_neutral_loss_list(line: &str) -> IResult<&str, Vec<NeutralLoss>> {
    many0(read_neutral_loss).parse(line)
}

fn parse_signed(line: &str) -> IResult<&str, i32> {
    alt((
        parse_i32,
        one_of("+-").map(|v| if v == '+' { 1 } else { -1 }),
    ))
    .parse(line)
}

fn parse_isotope_element(line: &str) -> IResult<&str, (i32, Box<str>)> {
    let (line, nucl) = parse_i32(line)?;
    let (line, (_, elt)) = parse_element_name(line)?;
    Ok((line, (nucl, elt)))
}

fn read_isotope(line: &str) -> IResult<&str, Isotope> {
    let (line, coefficient) = parse_signed.parse(line)?;
    let (line, _) = char('i')(line)?;
    if let Ok((line, (nucleon_count, element))) = parse_isotope_element(line) {
        Ok((
            line,
            Isotope::Element {
                coefficient,
                nucleon_count,
                element,
            },
        ))
    } else if let Ok((line, _)) = char::<_, nom::error::Error<&str>>('A')(line) {
        Ok((line, Isotope::Averaged { coefficient }))
    } else {
        Ok((line, Isotope::Basic { coefficient }))
    }
}

fn read_adduct(line: &str) -> IResult<&str, Adduct> {
    let (line, content) = context("adduct", nested('[', ']')).parse(&line)?;
    if content.is_empty() {
        return Ok((line, Adduct(Vec::new())));
    }
    let line = &content[1..content.len().saturating_sub(1)];

    let (line, _) = char('M').parse(line)?;

    let (line, components) = many1(parse_signed_formula_component).parse(line)?;
    Ok((line, Adduct(components)))
}

fn read_charge(line: &str) -> IResult<&str, i32> {
    let (line, _) = char('^')(line)?;
    let (line, z) = parse_i32.parse(line)?;
    Ok((line, z))
}

fn read_mass_error(line: &str) -> IResult<&str, Tolerance> {
    let (line, _) = char('/').parse(line)?;
    let (line, deviation) = recognize_float_or_exceptions(line)?;
    let deviation = deviation.parse::<f64>().unwrap();
    let (line, ppm) = opt(tag_no_case("ppm")).parse(line)?;
    if ppm.is_some() {
        Ok((line, Tolerance::PPM(deviation)))
    } else {
        Ok((line, Tolerance::Da(deviation)))
    }
}

fn read_confidence(line: &str) -> IResult<&str, f64> {
    let (rest, _) = char('*').parse(line)?;
    let (rest, conf) = recognize_float_or_exceptions(rest)?;
    let conf = conf.parse().unwrap();
    Ok((rest, conf))
}

pub fn parse_annotation_line_once(line: &str) -> IResult<&str, PeakAnnotation> {
    let (line, is_auxiliary) = context("parsing auxiliary marker", read_auxiliary).parse(line)?;
    let (line, analyte_number) = context("parsing analyte reference", read_analyte_reference).parse(line)?;
    let (line, ion) = context("parsing ion type", read_ion_type).parse(line)?;
    let (line, neutral_losses) = context("parsing neutral losses",read_neutral_loss_list).parse(line)?;
    let (line, isotopes) = context("parsing isotopes", many0(read_isotope)).parse(line)?;
    let (line, adduct) = context("parsing adduct", opt(read_adduct)).parse(line)?;
    let (line, charge) = if line.is_empty() {
        (line, Some(1))
    } else {
        context("parsing charge state", opt(read_charge)).parse(line)?
    };

    let (line, deviation) = context("parsing mass error", opt(read_mass_error)).parse(line)?;
    let (line, confidence) = context("parsing confidence", opt(read_confidence)).parse(line)?;

    Ok((
        line,
        PeakAnnotation::new(
            is_auxiliary,
            analyte_number.unwrap_or(1),
            ion,
            neutral_losses,
            isotopes,
            adduct,
            charge.unwrap_or(1),
            deviation,
            confidence,
        ),
    ))
}

pub fn parse_annotation_line(line: &str) -> IResult<&str, Vec<PeakAnnotation>> {
    all_consuming(separated_list0(char(','), parse_annotation_line_once)).parse(line)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_nested() {
        let (_, part) = nested('[', ']')("[foobar]baz").unwrap();
        assert_eq!(part, "[foobar]");

        let (_, part) = nested('[', ']')("[foo[bar]]baz").unwrap();
        assert_eq!(part, "[foo[bar]]");
    }

    #[test]
    fn test_loss() {
        let (_, loss) = read_neutral_loss("-H2O").unwrap();
        let x = NeutralLoss::Formula {
            coefficient: -1,
            formula: vec![
                FormulaComponent {
                    count: 2,
                    isotope: None,
                    element: Box::from("H"),
                },
                FormulaComponent {
                    count: 1,
                    isotope: None,
                    element: Box::from("O"),
                },
            ],
        };
        assert_eq!(loss, x);
    }

    #[test]
    fn test_parse_annot_peptide() {
        let (_, ion) = parse_annotation_line_once("m5:8-H2O/14.4ppm").unwrap();
        eprintln!("{ion}");
        let (_, ion) = parse_annotation_line_once("y5/5ppm*0.99").unwrap();
        eprintln!("{ion}");

        let (_, ion) = parse_annotation_line_once("b6-C2H5NOS-H2O^3/-0.0014").unwrap();
        eprintln!("{ion}");

        let (_, ion) = parse_annotation_line_once("b4+i/4.8ppm").unwrap();
        eprintln!("{ion}");
    }

    #[test]
    fn test_comma_separated_list() {
        let (_, ions) = parse_annotation_line("b4+i/4.8ppm,y5/5ppm*0.99").unwrap();
        eprintln!("{ions:?}")
    }

    #[test]
    fn test_immonium() {
        let (_, ion) = parse_annotation_line_once("IA").unwrap();
        eprintln!("{ion:?}");
        let err = parse_annotation_line_once("I?/5ppm").unwrap_err();
        eprintln!("{err}")
    }
}
