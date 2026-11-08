#![cfg(test)]

use super::*;
use crate::constants::{ColumnType, StmtExecuteParamFlags};

fn text(value: &'static str) -> &'static [u8] {
    value.as_bytes()
}

fn serialize<T: MySerialize>(value: &T) -> Vec<u8> {
    let mut buf = Vec::new();
    value.serialize(&mut buf);
    buf
}

fn build<'a>(
    params: impl IntoIterator<Item = StmtParam<'a>>,
    max_allowed_packet: usize,
) -> StmtExecuteRequest<'a> {
    StmtExecuteRequestBuilder::new()
        .build(params, max_allowed_packet)
        .unwrap()
}

fn build_with_flavor<'a>(
    params: impl IntoIterator<Item = StmtParam<'a>>,
    max_allowed_packet: usize,
    flavor: ServerFlavor,
) -> StmtExecuteRequest<'a> {
    StmtExecuteRequestBuilder::new()
        .server_flavor(flavor)
        .build(params, max_allowed_packet)
        .unwrap()
}

/// Binds `request` to the statement id used by these tests.
fn command<'a>(request: &StmtExecuteRequest<'a>) -> ComStmtExecute<'a> {
    ComStmtExecute::new(7, request.clone())
}

fn params_section(request: &StmtExecuteRequest<'_>) -> Vec<u8> {
    serialize(&command(request))[10..].to_vec()
}

/// `(param_index, chunk)` pairs of the long data packets.
fn long_data_packets(request: &StmtExecuteRequest<'_>) -> Vec<(u16, Vec<u8>)> {
    command(request)
        .long_data()
        .map(|packet| {
            let bytes = serialize(&packet);
            (
                u16::from_le_bytes([bytes[5], bytes[6]]),
                bytes[7..].to_vec(),
            )
        })
        .collect()
}

fn assert_typed(
    param: StmtParam<'_>,
    expected_type: ColumnType,
    expected_flags: StmtExecuteParamFlags,
    expected_value: &[u8],
) {
    let bitmap = u8::from(
        param.data() == &StmtParamData::Null && param.long_data_policy() != LongDataMode::External,
    );
    let request = build([param], 1024);
    assert_eq!(request.params().types(), &[(expected_type, expected_flags)]);

    let mut expected = vec![bitmap, 0x01, expected_type as u8, expected_flags.bits()];
    expected.extend_from_slice(expected_value);
    assert_eq!(params_section(&request), expected);
}

#[test]
fn empty_request_has_no_params_section() {
    let request = build([], 10);
    assert!(request.params().is_empty());
    assert_eq!(
        serialize(&command(&request)),
        vec![
            Command::COM_STMT_EXECUTE as u8,
            7,
            0,
            0,
            0,
            CursorType::CURSOR_TYPE_NO_CURSOR.bits(),
            1,
            0,
            0,
            0,
        ]
    );
    assert_eq!(serialize(request.params()), Vec::<u8>::new());
}

#[test]
fn empty_request_respects_packet_limit() {
    assert!(matches!(
        StmtExecuteRequestBuilder::new().build([], 9),
        Err(StmtExecuteError::PayloadTooLarge {
            needed: 10,
            limit: 9
        })
    ));
}

#[test]
fn rejects_too_small_packet_limit() {
    assert_eq!(
        StmtExecuteRequestBuilder::new().build([StmtParam::null()], 7),
        Err(StmtExecuteError::PacketSizeTooSmall { value: 7 })
    );
}

#[test]
fn rejects_too_many_params() {
    let params = (0..=u16::MAX as usize)
        .map(|_| StmtParam::null())
        .collect::<Vec<_>>();
    assert_eq!(
        StmtExecuteRequestBuilder::new().build(params, 1024),
        Err(StmtExecuteError::TooManyParams { count: 65536 })
    );
}

#[test]
fn encodes_each_param_type() {
    assert_typed(
        StmtParam::null(),
        ColumnType::MYSQL_TYPE_NULL,
        StmtExecuteParamFlags::empty(),
        &[],
    );
    assert_typed(
        StmtParam::bytes(text("abc")),
        ColumnType::MYSQL_TYPE_VAR_STRING,
        StmtExecuteParamFlags::empty(),
        &[3, b'a', b'b', b'c'],
    );
    assert_typed(
        StmtParam::blob(&[0u8, 1, 2][..]),
        ColumnType::MYSQL_TYPE_BLOB,
        StmtExecuteParamFlags::empty(),
        &[3, 0, 1, 2],
    );
    assert_typed(
        StmtParam::decimal(text("1.50")),
        ColumnType::MYSQL_TYPE_NEWDECIMAL,
        StmtExecuteParamFlags::empty(),
        &[4, b'1', b'.', b'5', b'0'],
    );
    assert_typed(
        StmtParam::int(-1),
        ColumnType::MYSQL_TYPE_LONGLONG,
        StmtExecuteParamFlags::empty(),
        &[0xff; 8],
    );
    assert_typed(
        StmtParam::uint(7),
        ColumnType::MYSQL_TYPE_LONGLONG,
        StmtExecuteParamFlags::UNSIGNED,
        &[7, 0, 0, 0, 0, 0, 0, 0],
    );
    assert_typed(
        StmtParam::float(1.5),
        ColumnType::MYSQL_TYPE_FLOAT,
        StmtExecuteParamFlags::empty(),
        &[0x00, 0x00, 0xc0, 0x3f],
    );
    assert_typed(
        StmtParam::double(2.5),
        ColumnType::MYSQL_TYPE_DOUBLE,
        StmtExecuteParamFlags::empty(),
        &[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x40],
    );
    assert_typed(
        StmtParam::date(2026, 11, 8),
        ColumnType::MYSQL_TYPE_DATE,
        StmtExecuteParamFlags::empty(),
        &[4, 0xea, 0x07, 11, 8],
    );
    assert_typed(
        StmtParam::datetime(2026, 11, 8, 12, 30, 0, 123456),
        ColumnType::MYSQL_TYPE_DATETIME,
        StmtExecuteParamFlags::empty(),
        &[11, 0xea, 0x07, 11, 8, 12, 30, 0, 0x40, 0xe2, 0x01, 0x00],
    );
    assert_typed(
        StmtParam::time(true, 1, 2, 3, 4, 5),
        ColumnType::MYSQL_TYPE_TIME,
        StmtExecuteParamFlags::empty(),
        &[12, 1, 1, 0, 0, 0, 2, 3, 4, 5, 0, 0, 0],
    );
    assert_typed(
        StmtParam::time(false, 0, 0, 0, 0, 0),
        ColumnType::MYSQL_TYPE_TIME,
        StmtExecuteParamFlags::empty(),
        &[0],
    );
    // A negative zero time is still encoded as a zero-length value.
    assert_typed(
        StmtParam::time(true, 0, 0, 0, 0, 0),
        ColumnType::MYSQL_TYPE_TIME,
        StmtExecuteParamFlags::empty(),
        &[0],
    );
}

#[test]
fn maps_json_according_to_server_flavor() {
    let request = build_with_flavor([StmtParam::json(text("{}"))], 1024, ServerFlavor::MySql);
    assert_eq!(
        request.params().types(),
        &[(ColumnType::MYSQL_TYPE_JSON, StmtExecuteParamFlags::empty())]
    );

    let request = build_with_flavor([StmtParam::json(text("{}"))], 1024, ServerFlavor::MariaDb);
    assert_eq!(
        request.params().types(),
        &[(
            ColumnType::MYSQL_TYPE_VAR_STRING,
            StmtExecuteParamFlags::empty()
        )]
    );
}

#[test]
fn as_type_checks_compatibility_and_range() {
    let request = build(
        [StmtParam::int(42).as_type(StmtParamType::Long).unwrap()],
        1024,
    );
    assert_eq!(
        params_section(&request),
        vec![0x00, 0x01, 0x03, 0x00, 42, 0, 0, 0]
    );

    assert_eq!(
        StmtParam::int(1 << 40)
            .as_type(StmtParamType::Long)
            .unwrap_err(),
        StmtParamError::OutOfRange {
            ty: StmtParamType::Long
        }
    );
    assert_eq!(
        StmtParam::int(-1)
            .as_type(StmtParamType::UTiny)
            .unwrap_err(),
        StmtParamError::OutOfRange {
            ty: StmtParamType::UTiny
        }
    );
    assert_eq!(
        StmtParam::uint(256)
            .as_type(StmtParamType::UTiny)
            .unwrap_err(),
        StmtParamError::OutOfRange {
            ty: StmtParamType::UTiny
        }
    );
    assert_eq!(
        StmtParam::bytes(text("x"))
            .as_type(StmtParamType::Long)
            .unwrap_err(),
        StmtParamError::IncompatibleType {
            ty: StmtParamType::Long,
            kind: "bytes"
        }
    );
    assert_eq!(
        StmtParam::double(1.0)
            .as_type(StmtParamType::Float)
            .unwrap_err(),
        StmtParamError::IncompatibleType {
            ty: StmtParamType::Float,
            kind: "double"
        }
    );
    assert_eq!(
        StmtParam::datetime(2026, 11, 8, 12, 0, 0, 0)
            .as_type(StmtParamType::Date)
            .unwrap_err(),
        StmtParamError::IncompatibleType {
            ty: StmtParamType::Date,
            kind: "datetime"
        }
    );
    // `null_as` is already typed, so `as_type` must keep it idempotent.
    assert!(
        StmtParam::null_as(StmtParamType::VarString)
            .as_type(StmtParamType::VarString)
            .is_ok()
    );
    assert_eq!(
        StmtParam::null()
            .as_type(StmtParamType::VarString)
            .unwrap_err(),
        StmtParamError::IncompatibleType {
            ty: StmtParamType::VarString,
            kind: "null"
        }
    );
    assert_eq!(
        StmtParam::raw(
            ColumnType::MYSQL_TYPE_LONG,
            StmtExecuteParamFlags::empty(),
            &[1, 0, 0, 0][..]
        )
        .as_type(StmtParamType::Long)
        .unwrap_err(),
        StmtParamError::IncompatibleType {
            ty: StmtParamType::Long,
            kind: "raw"
        }
    );
}

#[test]
fn as_type_converts_data_between_compatible_types() {
    let request = build(
        [
            StmtParam::date(2026, 11, 8)
                .as_type(StmtParamType::DateTime)
                .unwrap(),
            StmtParam::datetime(2026, 11, 8, 0, 0, 0, 0)
                .as_type(StmtParamType::Date)
                .unwrap(),
            StmtParam::float(1.5)
                .as_type(StmtParamType::Double)
                .unwrap(),
            StmtParam::int(1).as_type(StmtParamType::UTiny).unwrap(),
        ],
        1024,
    );

    let params = params_section(&request);
    assert_eq!(
        params,
        vec![
            0x00, 0x01, // bitmap, new params
            0x0c, 0x00, // datetime
            0x0a, 0x00, // date
            0x05, 0x00, // double
            0x01, 0x80, // tiny unsigned
            4, 0xea, 0x07, 11, 8, // datetime (only date part)
            4, 0xea, 0x07, 11, 8, // date
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x3f, // 1.5 as f64
            1,    // u8
        ]
    );
}

#[test]
fn validates_date_and_time_fields() {
    assert_eq!(
        StmtExecuteRequestBuilder::new()
            .build([StmtParam::date(2026, 13, 1)], 1024)
            .unwrap_err(),
        StmtExecuteError::InvalidParam {
            index: 0,
            field: "month"
        }
    );
    assert_eq!(
        StmtExecuteRequestBuilder::new()
            .build([StmtParam::time(false, 0, 24, 0, 0, 0)], 1024)
            .unwrap_err(),
        StmtExecuteError::InvalidParam {
            index: 0,
            field: "hours"
        }
    );
    assert_eq!(
        StmtExecuteRequestBuilder::new()
            .build([StmtParam::datetime(2026, 1, 1, 0, 0, 0, 1_000_000)], 1024)
            .unwrap_err(),
        StmtExecuteError::InvalidParam {
            index: 0,
            field: "micros"
        }
    );
}

#[test]
fn null_bitmap_layout() {
    let request = build([StmtParam::null()], 1024);
    assert_eq!(request.params().bitmap(), &[0x01]);

    let request = build(
        (0..8).map(|index| {
            if index == 0 {
                StmtParam::null()
            } else {
                StmtParam::int(index)
            }
        }),
        1024,
    );
    assert_eq!(request.params().bitmap(), &[0x01]);

    let request = build((0..8).map(|_| StmtParam::null()), 1024);
    assert_eq!(request.params().bitmap(), &[0xff]);

    let request = build(
        (0..9).map(|index| {
            if index == 8 {
                StmtParam::null()
            } else {
                StmtParam::int(index as i64)
            }
        }),
        1024,
    );
    assert_eq!(request.params().bitmap(), &[0x00, 0x01]);

    let request = build(
        (0..16).map(|index| {
            if index == 7 || index == 8 {
                StmtParam::null()
            } else {
                StmtParam::int(index as i64)
            }
        }),
        1024,
    );
    assert_eq!(request.params().bitmap(), &[0x80, 0x01]);

    let request = build(
        (0..7).map(|index| {
            if index == 6 {
                StmtParam::null()
            } else {
                StmtParam::int(index as i64)
            }
        }),
        1024,
    );
    assert_eq!(request.params().bitmap(), &[0x40]);
}

#[test]
fn null_as_keeps_explicit_type() {
    let request = build([StmtParam::null_as(StmtParamType::VarString)], 1024);
    assert!(request.params().is_null(0));
    assert_eq!(
        request.params().types(),
        &[(
            ColumnType::MYSQL_TYPE_VAR_STRING,
            StmtExecuteParamFlags::empty()
        )]
    );
    assert_eq!(params_section(&request), vec![0x01, 0x01, 0xfd, 0x00]);
}

#[test]
fn auto_demotes_only_large_values() {
    let request = build(
        [
            StmtParam::bytes(vec![0xab; 64]),
            StmtParam::bytes(text("small")),
            StmtParam::int(42),
        ],
        32,
    );

    assert!(request.params().is_long_data(0));
    assert!(!request.params().is_long_data(1));
    assert!(!request.params().is_long_data(2));

    let section = params_section(&request);
    assert!(!section.windows(64).any(|window| window == [0xab; 64]));
    assert!(section.ends_with(&[5, b's', b'm', b'a', b'l', b'l', 42, 0, 0, 0, 0, 0, 0, 0]));

    let long_data = command(&request)
        .long_data()
        .map(|packet| serialize(&packet))
        .collect::<Vec<_>>();
    assert_eq!(long_data.len(), 3);
    let payload = long_data
        .iter()
        .flat_map(|packet| packet[7..].iter().copied())
        .collect::<Vec<_>>();
    assert_eq!(payload, vec![0xab; 64]);
}

#[test]
fn auto_never_demotes_raw_and_reports_payload_size() {
    let big = vec![0xab; 64];
    assert_eq!(
        StmtExecuteRequestBuilder::new()
            .build(
                [StmtParam::raw(
                    ColumnType::MYSQL_TYPE_BLOB,
                    StmtExecuteParamFlags::empty(),
                    big
                )],
                30
            )
            .unwrap_err(),
        StmtExecuteError::PayloadTooLarge {
            needed: 78,
            limit: 30
        }
    );
}

#[test]
fn auto_reports_payload_size_when_nothing_can_be_demoted() {
    assert_eq!(
        StmtExecuteRequestBuilder::new()
            .build((0..4).map(|_| StmtParam::int(1)), 14)
            .unwrap_err(),
        StmtExecuteError::PayloadTooLarge {
            needed: 52,
            limit: 14
        }
    );
}

#[test]
fn always_and_never_modes() {
    let request = build(
        [StmtParam::blob(text("x")).long_data(LongDataMode::Always)],
        1024,
    );
    assert!(request.params().is_long_data(0));
    assert_eq!(params_section(&request), vec![0x00, 0x01, 0xfc, 0x00]);
    assert_eq!(command(&request).long_data().count(), 1);

    assert_eq!(
        StmtExecuteRequestBuilder::new()
            .build(
                [StmtParam::blob(vec![0xab; 64]).long_data(LongDataMode::Never)],
                30
            )
            .unwrap_err(),
        StmtExecuteError::PayloadTooLarge {
            needed: 79,
            limit: 30
        }
    );
}

#[test]
fn empty_value_as_long_data_yields_empty_chunk() {
    let request = build(
        [StmtParam::blob(&[0u8; 0][..]).long_data(LongDataMode::Always)],
        1024,
    );
    let packets = command(&request)
        .long_data()
        .map(|packet| serialize(&packet))
        .collect::<Vec<_>>();
    assert_eq!(packets, vec![vec![0x18, 7, 0, 0, 0, 0, 0]]);
}

#[test]
fn long_data_chunks_respect_packet_budget() {
    let request = build(
        [StmtParam::blob(vec![0xab; 8]).long_data(LongDataMode::Always)],
        14,
    );
    let packets = command(&request)
        .long_data()
        .map(|packet| serialize(&packet))
        .collect::<Vec<_>>();
    assert_eq!(packets.len(), 2);
    assert_eq!(packets[0].len(), 7 + 7);
    assert_eq!(packets[1].len(), 7 + 1);
}

#[test]
fn external_long_data_is_excluded_and_reported() {
    let request = build(
        [
            StmtParam::external_long_data(StmtParamType::Blob),
            StmtParam::int(42),
        ],
        1024,
    );

    assert!(request.params().is_long_data(0));
    assert!(request.params().is_external_long_data(0));
    assert!(!request.params().is_null(0));
    assert_eq!(
        request
            .params()
            .external_long_data_params()
            .collect::<Vec<_>>(),
        vec![0]
    );
    assert_eq!(command(&request).long_data().count(), 0);

    let section = params_section(&request);
    assert_eq!(
        section,
        vec![0x00, 0x01, 0xfc, 0x00, 0x08, 0x00, 42, 0, 0, 0, 0, 0, 0, 0]
    );
}

#[test]
fn external_long_data_rejects_carried_value() {
    assert_eq!(
        StmtExecuteRequestBuilder::new()
            .build(
                [StmtParam::blob(vec![1, 2, 3]).long_data(LongDataMode::External)],
                1024
            )
            .unwrap_err(),
        StmtExecuteError::ExternalWithData { index: 0 }
    );
    assert_eq!(
        StmtExecuteRequestBuilder::new()
            .build(
                [StmtParam::bytes(text("x")).long_data(LongDataMode::External)],
                1024
            )
            .unwrap_err(),
        StmtExecuteError::ExternalWithData { index: 0 }
    );

    let request = build([StmtParam::external_long_data(StmtParamType::Blob)], 1024);
    assert!(request.params().is_external_long_data(0));

    let request = build(
        [StmtParam::blob(text("x")).long_data(LongDataMode::Always)],
        1024,
    );
    assert!(!request.params().is_external_long_data(0));
}

#[test]
fn long_data_iterator_is_lazy_and_ordered() {
    let request = build(
        [
            StmtParam::blob(vec![1u8; 16]).long_data(LongDataMode::Always),
            StmtParam::blob(vec![2u8; 3]).long_data(LongDataMode::Always),
            StmtParam::blob(&[0u8; 0][..]).long_data(LongDataMode::Always),
        ],
        18,
    );

    assert_eq!(
        request.params().serialized_len(),
        command(&request).serialized_len() - 10
    );
    assert_eq!(
        long_data_packets(&request),
        vec![
            (0, vec![1; 11]),
            (0, vec![1; 5]),
            (1, vec![2; 3]),
            (2, vec![]),
        ]
    );
}

#[test]
fn serialized_len_matches_serialization() {
    let request = build([], 10);
    assert_eq!(
        command(&request).serialized_len(),
        serialize(&command(&request)).len()
    );
    assert_eq!(request.params().serialized_len(), 0);

    let request = build(
        [
            StmtParam::null(),
            StmtParam::bytes(text("abc")),
            StmtParam::int(1),
        ],
        1024,
    );
    assert_eq!(
        command(&request).serialized_len(),
        serialize(&command(&request)).len()
    );
    assert_eq!(
        request.params().serialized_len(),
        serialize(request.params()).len()
    );

    let request = build(
        [
            StmtParam::blob(vec![9; 32]).long_data(LongDataMode::Always),
            StmtParam::external_long_data(StmtParamType::Blob),
            StmtParam::null_as(StmtParamType::VarString),
        ],
        1024,
    );
    assert_eq!(
        command(&request).serialized_len(),
        serialize(&command(&request)).len()
    );
    assert_eq!(
        request.params().serialized_len(),
        serialize(request.params()).len()
    );

    let mut request = build([StmtParam::int(1), StmtParam::double(2.5)], 1024);
    let metadata = request.metadata();
    request.reuse_types(&metadata).unwrap();
    assert_eq!(
        command(&request).serialized_len(),
        serialize(&command(&request)).len()
    );
    assert_eq!(
        request.params().serialized_len(),
        serialize(request.params()).len()
    );
}

#[test]
fn into_metadata_moves_types() {
    let request = build([StmtParam::int(1), StmtParam::bytes(text("x"))], 1024);
    let expected = request.metadata();
    let actual = request.into_metadata();
    assert_eq!(actual, expected);

    let mut next = build([StmtParam::int(2), StmtParam::bytes(text("y"))], 1024);
    next.reuse_types(&actual).unwrap();
    assert!(!next.params().new_params_bound());
    assert_eq!(
        command(&next).serialized_len(),
        serialize(&command(&next)).len()
    );
}

#[test]
fn long_data_is_rejected_for_incompatible_params() {
    assert_eq!(
        StmtExecuteRequestBuilder::new()
            .build([StmtParam::int(1).long_data(LongDataMode::Always)], 1024)
            .unwrap_err(),
        StmtExecuteError::LongDataNotSupported {
            index: 0,
            ty: StmtParamType::LongLong
        }
    );
    assert_eq!(
        StmtExecuteRequestBuilder::new()
            .build([StmtParam::null().long_data(LongDataMode::Always)], 1024)
            .unwrap_err(),
        StmtExecuteError::NullWithLongData { index: 0 }
    );
    assert_eq!(
        StmtExecuteRequestBuilder::new()
            .build([StmtParam::external_long_data(StmtParamType::Null)], 1024)
            .unwrap_err(),
        StmtExecuteError::NullWithLongData { index: 0 }
    );
    assert_eq!(
        StmtExecuteRequestBuilder::new()
            .build(
                [StmtParam::raw(
                    ColumnType::MYSQL_TYPE_BLOB,
                    StmtExecuteParamFlags::empty(),
                    text("x"),
                )
                .long_data(LongDataMode::Always)],
                1024
            )
            .unwrap_err(),
        StmtExecuteError::RawWithLongData { index: 0 }
    );
}

#[test]
fn json_long_data_depends_on_server_flavor() {
    assert_eq!(
        StmtExecuteRequestBuilder::new()
            .build(
                [StmtParam::json(text("{}")).long_data(LongDataMode::Always)],
                1024
            )
            .unwrap_err(),
        StmtExecuteError::LongDataNotSupported {
            index: 0,
            ty: StmtParamType::Json
        }
    );

    let request = build_with_flavor(
        [StmtParam::json(text("{}")).long_data(LongDataMode::Always)],
        1024,
        ServerFlavor::MariaDb,
    );
    assert_eq!(
        request.params().types()[0].0,
        ColumnType::MYSQL_TYPE_VAR_STRING
    );
    assert_eq!(command(&request).long_data().count(), 1);
}

#[test]
fn reuse_types_checks_metadata() {
    let mut request = build([StmtParam::int(1), StmtParam::bytes(text("x"))], 1024);
    let metadata = request.metadata();
    assert_eq!(metadata.types(), request.params().types());
    assert_eq!(metadata.len(), 2);
    assert!(request.params().new_params_bound());

    request.reuse_types(&metadata).unwrap();
    assert!(!request.params().new_params_bound());

    let section = params_section(&request);
    assert_eq!(section[0], 0x00);
    assert_eq!(section[1], 0x00);
    assert_eq!(section[2..], [1, 0, 0, 0, 0, 0, 0, 0, 1, b'x']);

    let mut request = build([StmtParam::int(1)], 1024);
    let shorter = ParamsMetadata {
        types: request.metadata().types[..0].to_vec(),
    };
    assert_eq!(
        request.reuse_types(&shorter).unwrap_err(),
        StmtExecuteMetadataError::ParamCountMismatch {
            expected: 0,
            actual: 1
        }
    );
    assert!(request.params().new_params_bound());

    let different_type = ParamsMetadata {
        types: vec![(
            ColumnType::MYSQL_TYPE_VAR_STRING,
            StmtExecuteParamFlags::empty(),
        )],
    };
    assert_eq!(
        request.reuse_types(&different_type).unwrap_err(),
        StmtExecuteMetadataError::TypeMismatch {
            index: 0,
            expected: (
                ColumnType::MYSQL_TYPE_VAR_STRING,
                StmtExecuteParamFlags::empty()
            ),
            actual: (
                ColumnType::MYSQL_TYPE_LONGLONG,
                StmtExecuteParamFlags::empty()
            ),
        }
    );
    assert!(request.params().new_params_bound());
}

#[test]
fn reuse_types_unchecked_checks_count() {
    let mut request = build([StmtParam::int(1)], 1024);
    let metadata = request.metadata();
    request.reuse_types_unchecked(&metadata).unwrap();
    assert!(!request.params().new_params_bound());

    let mut request = build([StmtParam::int(1)], 1024);
    let wrong_count = ParamsMetadata { types: Vec::new() };
    assert_eq!(
        request.reuse_types_unchecked(&wrong_count).unwrap_err(),
        StmtExecuteMetadataError::ParamCountMismatch {
            expected: 0,
            actual: 1
        }
    );
    assert!(request.params().new_params_bound());
}

#[test]
fn null_and_value_types_do_not_reuse() {
    let request = build([StmtParam::null()], 1024);
    let metadata = request.metadata();

    let mut next = build([StmtParam::bytes(text("x"))], 1024);
    assert!(matches!(
        next.reuse_types(&metadata),
        Err(StmtExecuteMetadataError::TypeMismatch { index: 0, .. })
    ));

    let mut request = build([StmtParam::null_as(StmtParamType::VarString)], 1024);
    request.reuse_types(&metadata).unwrap_err();

    let request = build([StmtParam::null_as(StmtParamType::VarString)], 1024);
    let metadata = request.metadata();
    let mut next = build([StmtParam::bytes(text("x"))], 1024);
    next.reuse_types(&metadata).unwrap();
    assert!(!next.params().new_params_bound());
}

#[test]
fn params_serialization_is_independent_of_header() {
    let request = build(
        [
            StmtParam::null(),
            StmtParam::bytes(text("abc")),
            StmtParam::int(42),
            StmtParam::blob(vec![1, 2, 3]).long_data(LongDataMode::Always),
        ],
        1024,
    );
    assert_eq!(serialize(request.params()), params_section(&request));
}

#[test]
fn request_accessors() {
    let request = StmtExecuteRequestBuilder::new()
        .cursor_type(CursorType::CURSOR_TYPE_READ_ONLY)
        .build([StmtParam::int(1)], 1024)
        .unwrap();

    assert_eq!(request.flags(), CursorType::CURSOR_TYPE_READ_ONLY);
    assert_eq!(request.iteration_count(), 1);
    assert_eq!(request.params().len(), 1);
    assert!(!request.params().is_empty());
    assert_eq!(
        request.params().column_type(0),
        Some((
            ColumnType::MYSQL_TYPE_LONGLONG,
            StmtExecuteParamFlags::empty()
        ))
    );
    assert_eq!(request.params().column_type(1), None);

    let command = ComStmtExecute::new(42, request);
    assert_eq!(command.stmt_id(), 42);
    assert_eq!(command.params().len(), 1);

    let serialized = serialize(&command);
    assert_eq!(serialized[0], Command::COM_STMT_EXECUTE as u8);
    assert_eq!(&serialized[1..5], &[42, 0, 0, 0]);
    assert_eq!(serialized[5], CursorType::CURSOR_TYPE_READ_ONLY.bits());
}

#[test]
fn command_rebinds_statement_id() {
    let request = build([StmtParam::int(1)], 1024);

    let command = ComStmtExecute::new(9, request);
    assert_eq!(command.stmt_id(), 9);
    assert_eq!(&serialize(&command)[1..5], &[9, 0, 0, 0]);
    assert_eq!(
        command.params().serialized_len(),
        command.serialized_len() - 10
    );

    let request = command.into_request();
    assert_eq!(request.params().len(), 1);
}

#[test]
fn com_stmt_reset_serialization() {
    let reset = ComStmtReset::new(42);
    assert_eq!(reset.stmt_id, 42);
    assert_eq!(
        serialize(&reset),
        vec![Command::COM_STMT_RESET as u8, 42, 0, 0, 0]
    );
}

#[test]
fn raw_param_is_written_as_is() {
    let request = build(
        [StmtParam::raw(
            ColumnType::MYSQL_TYPE_LONG,
            StmtExecuteParamFlags::empty(),
            &[1, 2, 3, 4][..],
        )],
        1024,
    );
    assert_eq!(
        params_section(&request),
        vec![0x00, 0x01, 0x03, 0x00, 1, 2, 3, 4]
    );
}

#[test]
fn value_conversions_preserve_legacy_wire_types() {
    let values = [
        Value::NULL,
        Value::Bytes(b"abc".to_vec()),
        Value::Int(42),
        Value::UInt(7),
        Value::Float(1.5),
        Value::Double(2.5),
        Value::Date(2026, 11, 8, 12, 30, 0, 123456),
        Value::Time(true, 1, 2, 3, 4, 5),
    ];

    let request = build(values.iter().map(StmtParam::from), 1024);
    assert_eq!(
        request.params().types(),
        &[
            (ColumnType::MYSQL_TYPE_NULL, StmtExecuteParamFlags::empty()),
            (
                ColumnType::MYSQL_TYPE_VAR_STRING,
                StmtExecuteParamFlags::empty()
            ),
            (
                ColumnType::MYSQL_TYPE_LONGLONG,
                StmtExecuteParamFlags::empty()
            ),
            (
                ColumnType::MYSQL_TYPE_LONGLONG,
                StmtExecuteParamFlags::UNSIGNED
            ),
            (ColumnType::MYSQL_TYPE_FLOAT, StmtExecuteParamFlags::empty()),
            (
                ColumnType::MYSQL_TYPE_DOUBLE,
                StmtExecuteParamFlags::empty()
            ),
            (
                ColumnType::MYSQL_TYPE_DATETIME,
                StmtExecuteParamFlags::empty()
            ),
            (ColumnType::MYSQL_TYPE_TIME, StmtExecuteParamFlags::empty()),
        ]
    );

    let owned = build(values.iter().cloned().map(StmtParam::from), 1024);
    assert_eq!(request.params().types(), owned.params().types());
}

#[test]
#[allow(deprecated)]
fn legacy_builder_keeps_wire_format() {
    use crate::packets::ComStmtExecuteRequestBuilder;

    let values = vec![
        Value::NULL,
        Value::Bytes(b"abc".to_vec()),
        Value::Int(42),
        Value::UInt(7),
        Value::Float(1.5),
        Value::Double(2.5),
        Value::Date(2026, 11, 8, 12, 30, 0, 123456),
        Value::Time(true, 1, 2, 3, 4, 5),
        Value::Bytes(Vec::new()),
        Value::Int(-1),
    ];

    let (request, as_long_data) = ComStmtExecuteRequestBuilder::new(4).build(&values);
    assert!(!as_long_data);

    let expected = vec![
        Command::COM_STMT_EXECUTE as u8,
        4,
        0,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        // null bitmap (10 params -> 2 bytes)
        0x01,
        0x00,
        // new params bound
        0x01,
        // types
        0x06,
        0x00,
        0xfd,
        0x00,
        0x08,
        0x00,
        0x08,
        0x80,
        0x04,
        0x00,
        0x05,
        0x00,
        0x0c,
        0x00,
        0x0b,
        0x00,
        0xfd,
        0x00,
        0x08,
        0x00,
        // values
        0x03,
        b'a',
        b'b',
        b'c',
        42,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        7,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0x00,
        0x00,
        0xc0,
        0x3f,
        0x00,
        0x00,
        0x00,
        0x00,
        0x00,
        0x00,
        0x04,
        0x40,
        0x0b,
        0xea,
        0x07,
        0x0b,
        0x08,
        0x0c,
        0x1e,
        0x00,
        0x40,
        0xe2,
        0x01,
        0x00,
        0x0c,
        0x01,
        0x01,
        0x00,
        0x00,
        0x00,
        0x02,
        0x03,
        0x04,
        0x05,
        0x00,
        0x00,
        0x00,
        0x00,
        0xff,
        0xff,
        0xff,
        0xff,
        0xff,
        0xff,
        0xff,
        0xff,
    ];

    assert_eq!(serialize(&request), expected);
}

#[test]
#[allow(deprecated)]
fn legacy_builder_uses_global_long_data() {
    use crate::packets::ComStmtExecuteRequestBuilder;

    let values = vec![Value::Bytes(vec![0xab; MAX_PAYLOAD_LEN + 1])];
    let (request, as_long_data) = ComStmtExecuteRequestBuilder::new(4).build(&values);
    assert!(as_long_data);

    let serialized = serialize(&request);
    // The value is excluded, so only the header, the bitmap and the type remain.
    assert_eq!(serialized.len(), 14);
    assert_eq!(&serialized[10..], &[0x00, 0x01, 0xfd, 0x00]);
}

#[test]
fn deserializes_com_stmt_execute() {
    let request = build(
        [
            StmtParam::null(),
            StmtParam::bytes(text("abc")),
            StmtParam::int(42),
            StmtParam::int(7).as_type(StmtParamType::Tiny).unwrap(),
            StmtParam::uint(9)
                .as_type(StmtParamType::ULongLong)
                .unwrap(),
            StmtParam::double(2.5),
            StmtParam::datetime(2026, 11, 8, 12, 30, 0, 123_456),
            StmtParam::time(true, 1, 2, 3, 4, 5),
        ],
        1024,
    );
    let command = ComStmtExecute::new(7, request);
    let bytes = serialize(&command);

    let ctx = ComStmtExecuteCtx::new(u16::try_from(command.params().len()).unwrap());
    let parsed = ComStmtExecute::deserialize(ctx, &mut ParseBuf(&bytes)).unwrap();

    assert_eq!(parsed.stmt_id(), 7);
    assert_eq!(parsed.request().flags(), command.request().flags());
    assert_eq!(
        parsed.request().iteration_count(),
        command.request().iteration_count()
    );
    assert_eq!(parsed.params().types(), command.params().types());
    assert!(parsed.params().is_null(0));
    assert!(!parsed.params().is_null(1));
    assert_eq!(
        parsed.params().serialized_len(),
        command.params().serialized_len()
    );
    assert_eq!(serialize(&parsed), bytes);
}

#[test]
fn deserializes_com_stmt_execute_without_types() {
    let mut request = build([StmtParam::int(1), StmtParam::bytes(text("x"))], 1024);
    let metadata = request.metadata();
    request.reuse_types(&metadata).unwrap();

    let command = ComStmtExecute::new(3, request);
    let bytes = serialize(&command);

    let ctx = ComStmtExecuteCtx {
        types: Some(metadata.types()),
        ..ComStmtExecuteCtx::new(2)
    };
    let parsed = ComStmtExecute::deserialize(ctx, &mut ParseBuf(&bytes)).unwrap();

    assert!(!parsed.params().new_params_bound());
    assert_eq!(parsed.params().types(), metadata.types());
    assert_eq!(serialize(&parsed), bytes);
}

#[test]
fn deserializes_com_stmt_execute_with_long_data() {
    let request = build(
        [
            StmtParam::blob(vec![1, 2, 3]).long_data(LongDataMode::Always),
            StmtParam::int(5),
        ],
        1024,
    );
    let command = ComStmtExecute::new(9, request);
    let bytes = serialize(&command);

    let long_data = [0_u16];
    let ctx = ComStmtExecuteCtx {
        long_data: &long_data,
        ..ComStmtExecuteCtx::new(2)
    };
    let parsed = ComStmtExecute::deserialize(ctx, &mut ParseBuf(&bytes)).unwrap();

    assert!(parsed.params().is_external_long_data(0));
    assert_eq!(parsed.long_data().count(), 0);
    assert_eq!(serialize(&parsed), bytes);
}

#[test]
fn com_stmt_execute_deserialization_errors() {
    let command = ComStmtExecute::new(7, build([StmtParam::int(1)], 1024));
    let bytes = serialize(&command);

    let mut bad_header = bytes.clone();
    bad_header[0] = Command::COM_STMT_RESET as u8;
    assert!(
        ComStmtExecute::deserialize(ComStmtExecuteCtx::new(1), &mut ParseBuf(&bad_header)).is_err()
    );

    let mut with_trailing = bytes.clone();
    with_trailing.push(0);
    assert!(
        ComStmtExecute::deserialize(ComStmtExecuteCtx::new(1), &mut ParseBuf(&with_trailing))
            .is_err()
    );

    let long_data = [1_u16];
    assert!(
        ComStmtExecute::deserialize(
            ComStmtExecuteCtx {
                long_data: &long_data,
                ..ComStmtExecuteCtx::new(1)
            },
            &mut ParseBuf(&bytes)
        )
        .is_err()
    );

    let mut request = build([StmtParam::int(1)], 1024);
    let metadata = request.metadata();
    request.reuse_types(&metadata).unwrap();
    let bytes = serialize(&ComStmtExecute::new(7, request));
    assert!(ComStmtExecute::deserialize(ComStmtExecuteCtx::new(1), &mut ParseBuf(&bytes)).is_err());
}

#[test]
fn converts_params_to_values() {
    let expected = vec![
        Value::NULL,
        Value::Bytes(b"abc".to_vec()),
        Value::Int(42),
        Value::Float(1.5),
        Value::Double(2.5),
        Value::Date(2026, 11, 8, 0, 0, 0, 0),
        Value::Date(2026, 11, 8, 12, 30, 0, 123_456),
        Value::Time(true, 1, 2, 3, 4, 5),
    ];

    let request = build(
        [
            StmtParam::null(),
            StmtParam::bytes(text("abc")),
            StmtParam::int(42),
            StmtParam::float(1.5),
            StmtParam::double(2.5),
            StmtParam::date(2026, 11, 8),
            StmtParam::datetime(2026, 11, 8, 12, 30, 0, 123_456),
            StmtParam::time(true, 1, 2, 3, 4, 5),
        ],
        1024,
    );
    assert_eq!(request.params().values().unwrap(), expected);

    let command = ComStmtExecute::new(7, request);
    let bytes = serialize(&command);
    let ctx = ComStmtExecuteCtx::new(u16::try_from(expected.len()).unwrap());
    let parsed = ComStmtExecute::deserialize(ctx, &mut ParseBuf(&bytes)).unwrap();

    assert_eq!(parsed.params().values().unwrap(), expected);
    assert_eq!(
        parsed.params().param(0).unwrap().value().unwrap(),
        Value::NULL
    );
    assert!(parsed.params().param(expected.len()).is_none());
}

#[test]
fn values_of_raw_and_long_data_params() {
    let raw = StmtParam::raw(
        ColumnType::MYSQL_TYPE_LONG,
        StmtExecuteParamFlags::empty(),
        42_i32.to_le_bytes().to_vec(),
    );
    let request = build([raw], 1024);
    assert_eq!(
        request.params().param(0).unwrap().value().unwrap(),
        Value::Int(42)
    );

    let request = build([StmtParam::external_long_data(StmtParamType::Blob)], 1024);
    assert!(request.params().values().is_err());

    let unsupported = StmtParam::raw(
        ColumnType::MYSQL_TYPE_TIMESTAMP2,
        StmtExecuteParamFlags::empty(),
        Vec::new(),
    );
    let request = build([unsupported], 1024);
    let err = request.params().param(0).unwrap().value().unwrap_err();
    assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
}

#[test]
fn deserializes_empty_com_stmt_execute() {
    let command = ComStmtExecute::new(1, build([], 10));
    let bytes = serialize(&command);
    let parsed =
        ComStmtExecute::deserialize(ComStmtExecuteCtx::new(0), &mut ParseBuf(&bytes)).unwrap();
    assert!(parsed.params().is_empty());
    assert_eq!(serialize(&parsed), bytes);
}

#[test]
fn deserializes_com_stmt_reset() {
    let reset = ComStmtReset::new(42);
    let bytes = serialize(&reset);
    let parsed = ComStmtReset::deserialize((), &mut ParseBuf(&bytes)).unwrap();
    assert_eq!(parsed, reset);
    assert_eq!(parsed.stmt_id, 42);
}

#[test]
fn deserializes_com_stmt_send_long_data() {
    let packet = ComStmtSendLongData::new(7, 2, &b"abc"[..]);
    let bytes = serialize(&packet);
    let parsed = ComStmtSendLongData::deserialize((), &mut ParseBuf(&bytes)).unwrap();
    assert_eq!(serialize(&parsed), bytes);
}
