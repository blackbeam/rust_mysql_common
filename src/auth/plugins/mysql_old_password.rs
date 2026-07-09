pub const SCRAMBLE_LENGTH_323: usize = 8;

#[derive(Debug, Default)]
pub struct MysqlOldPassword(());

impl super::ChallengeResponsePlugin for MysqlOldPassword {
    fn run<C: super::Context>(
        &mut self,
        ctx: C,
        challenge: &[u8],
    ) -> Result<super::Response, super::Error> {
        // We're going to use first eight bytes of the challenge here
        let Some(challenge) = challenge.get(..SCRAMBLE_LENGTH_323) else {
            return Err(super::Error::Challenge);
        };
        let Ok(scramble) = challenge.try_into() else {
            return Err(super::Error::Challenge);
        };

        let response = scramble_323(scramble, ctx.pass())
            .map(|x| x.to_vec())
            .unwrap_or_default();

        Ok(super::Response::Last {
            packet: Some(response),
        })
    }

    fn password_hash<C: super::Context>(&self, _ctx: C) -> Option<Vec<u8>> {
        None
    }
}

/// Old (pre 4.1) authentication plugin
pub fn scramble_323(nonce: &[u8; SCRAMBLE_LENGTH_323], password: &[u8]) -> Option<[u8; 8]> {
    struct Rand323 {
        seed1: u32,
        seed2: u32,
        max_value: u32,
        max_value_dbl: f64,
    }

    impl Rand323 {
        fn init(seed1: u32, seed2: u32) -> Self {
            Self {
                max_value: 0x3FFFFFFF,
                max_value_dbl: 0x3FFFFFFF as f64,
                seed1: seed1 % 0x3FFFFFFF,
                seed2: seed2 % 0x3FFFFFFF,
            }
        }

        fn my_rnd(&mut self) -> f64 {
            self.seed1 = (self.seed1 * 3 + self.seed2) % self.max_value;
            self.seed2 = (self.seed1 + self.seed2 + 33) % self.max_value;
            (self.seed1 as f64) / self.max_value_dbl
        }
    }

    let mut hash_pass = [0_u32; 2];
    let mut hash_message = [0_u32; 2];

    if password.is_empty() {
        return None;
    }

    let mut output = [0_u8; 8];

    hash_password(&mut hash_pass, password);
    hash_password(&mut hash_message, nonce);

    let mut rand_st = Rand323::init(
        hash_pass[0] ^ hash_message[0],
        hash_pass[1] ^ hash_message[1],
    );

    for x in output.iter_mut() {
        *x = ((rand_st.my_rnd() * 31_f64).floor() + 64_f64) as u8;
    }

    let extra = (rand_st.my_rnd() * 31_f64).floor() as u8;

    for x in output.iter_mut() {
        *x ^= extra;
    }

    Some(output)
}

/// Insecure password hasing used in mysql_old_password.
pub fn hash_password(output: &mut [u32; 2], password: &[u8]) {
    let mut nr: u32 = 1345345333;
    let mut add: u32 = 7;
    let mut nr2: u32 = 0x12345671;

    let mut tmp: u32;

    for x in password {
        if *x == b' ' || *x == b'\t' {
            continue;
        }

        tmp = *x as u32;
        nr ^= (nr & 63)
            .wrapping_add(add)
            .wrapping_mul(tmp)
            .wrapping_add(nr << 8);
        nr2 = nr2.wrapping_add((nr2 << 8) ^ nr);
        add = add.wrapping_add(tmp);
    }

    output[0] = nr & 0b01111111_11111111_11111111_11111111;
    output[1] = nr2 & 0b01111111_11111111_11111111_11111111;
}
