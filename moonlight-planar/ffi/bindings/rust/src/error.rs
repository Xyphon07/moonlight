use std::error::Error;
use std::fmt::{Display, Formatter};

use crate::raw_generated::{ML_STATUS_OK, NativeObstruction};

#[derive(Debug, Clone, PartialEq)]
pub enum MoonlightError {
    RuntimeInitializationRefused {
        status: u32,
    },
    AbiVersionMismatch {
        expected: u32,
        observed: u32,
    },
    AbiObstruction(AbiObstruction),
    InvalidNativeResult {
        message: String,
    },
    UnknownWireValue {
        vocabulary: WireVocabulary,
        value: u32,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbiObstruction {
    pub status: u32,
    pub code: u32,
    pub coordinate_error: u32,
    pub input_index: u64,
    pub first_index: u64,
    pub second_index: u64,
    pub first_value: f64,
    pub second_value: f64,
    pub point: [f64; 2],
    pub message: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WireVocabulary {
    RegionLocation,
    MinkowskiOperation,
}

impl Display for MoonlightError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RuntimeInitializationRefused { status } => {
                write!(
                    formatter,
                    "Moonlight runtime initialization refused with status {status}"
                )
            }
            Self::AbiVersionMismatch { expected, observed } => write!(
                formatter,
                "Moonlight ABI {observed} is incompatible with expected ABI {expected}"
            ),
            Self::AbiObstruction(obstruction) => write!(formatter, "{obstruction}"),
            Self::InvalidNativeResult { message } => write!(formatter, "{message}"),
            Self::UnknownWireValue { vocabulary, value } => {
                write!(
                    formatter,
                    "Moonlight returned unknown {vocabulary} value {value}"
                )
            }
        }
    }
}

impl Display for AbiObstruction {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        if self.message.is_empty() {
            write!(
                formatter,
                "Moonlight ABI failure {}:{}",
                self.status, self.code
            )
        } else {
            write!(formatter, "{}", self.message)
        }
    }
}

impl Display for WireVocabulary {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RegionLocation => write!(formatter, "region-location"),
            Self::MinkowskiOperation => write!(formatter, "minkowski-operation"),
        }
    }
}

impl Error for MoonlightError {}

pub(crate) fn status_result(
    status: u32,
    obstruction: NativeObstruction,
) -> Result<(), MoonlightError> {
    if status == ML_STATUS_OK {
        Ok(())
    } else {
        Err(MoonlightError::AbiObstruction(AbiObstruction::from_native(
            status,
            obstruction,
        )))
    }
}

impl AbiObstruction {
    fn from_native(status: u32, obstruction: NativeObstruction) -> Self {
        let message_end = obstruction
            .message
            .iter()
            .position(|character| *character == 0)
            .unwrap_or(obstruction.message.len());
        let message_bytes = obstruction
            .message
            .get(..message_end)
            .unwrap_or_default()
            .iter()
            .map(|character| *character as u8)
            .collect::<Vec<_>>();
        Self {
            status,
            code: obstruction.code,
            coordinate_error: obstruction.coordinate_error,
            input_index: obstruction.input_index,
            first_index: obstruction.first_index,
            second_index: obstruction.second_index,
            first_value: obstruction.first_value,
            second_value: obstruction.second_value,
            point: [obstruction.point_x, obstruction.point_y],
            message: String::from_utf8_lossy(&message_bytes).into_owned(),
        }
    }
}

pub(crate) fn projection_shape_error() -> MoonlightError {
    invalid_native_result("Moonlight returned a malformed bulk projection")
}

pub(crate) fn success_without_handle_error() -> MoonlightError {
    invalid_native_result("Moonlight returned success without a handle")
}

pub(crate) fn invalid_native_result(message: impl Into<String>) -> MoonlightError {
    MoonlightError::InvalidNativeResult {
        message: message.into(),
    }
}
