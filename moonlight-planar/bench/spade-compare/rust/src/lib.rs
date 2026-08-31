//! Cross-language benchmark input programs shared by Rust interpreters.

use spade::Point2;

/// The Haskell generator, bit for bit.
pub fn random_points(seed: u64, count: usize) -> Vec<Point2<f64>> {
    const A: u64 = 6364136223846793005;
    const C: u64 = 1442695040888963407;
    fn unit(value: u64) -> f64 {
        (value >> 11) as f64 / 9_007_199_254_740_992.0
    }
    let mut state = seed;
    (0..count)
        .map(|_| {
            let s1 = state.wrapping_mul(A).wrapping_add(C);
            let s2 = s1.wrapping_mul(A).wrapping_add(C);
            state = s2;
            Point2::new(2.0 * unit(s1) - 1.0, 2.0 * unit(s2) - 1.0)
        })
        .collect()
}
