extern crate hound;

use std::i16;
use std::f32;

// Comparing band-limited to non-band-limited synthesis

const SAMPLE_RATE: u32 = 44100;

// Geometric square wave alternates between min and max amplitudes
fn geometric_square(rate: f32, length: f32) -> Vec<f32> {
  // How many samples do we need?
  let samples = length * (SAMPLE_RATE as f32);

  // How many periods per sample?
  let ratio = rate / (SAMPLE_RATE as f32);

  (0..(samples as u32)).map(|s| {
    // t in [0,1] is a time index into a wave period
    let t = ratio * (s as f32) % 1.0;
    if t < 0.5 { -1.0 }
    else { 1.0 }
  }).collect()
}

// Square wave from summing odd harmonics of a Fourier series
fn fourier_square(rate: f32, length: f32) -> Vec<f32> {
  // How many samples do we need?
  let samples = length * (SAMPLE_RATE as f32);

  // How many periods per sample?
  let ratio = rate / (SAMPLE_RATE as f32);

  (0..(samples as u32)).map(|s| {
    // Sum the Fourier series until the Nyquist barrier
    (0u32..)
    // Only odd harmonics
      .map(|k| (2 * k + 1) as f32)
    // Limit to frequencies below Nyquist
      .take_while(|h| (h * rate) < ((SAMPLE_RATE as f32) / 2.0))
      .map(|h| {
        // t in [0,1] is a time index into a wave period
        let t = ratio * (s as f32);
        f32::sin(2.0 * f32::consts::PI * h * t) / h
      }).sum()
  }).collect()
}

fn main() {
  // Prepare the WAV writer
  let spec = hound::WavSpec {
    channels: 1,
    sample_rate: SAMPLE_RATE,
    bits_per_sample: 16,
    sample_format: hound::SampleFormat::Int,
  };
  let max = i16::MAX as f32;
  let amp = 0.2;
  let rate = 440.0;
  let length = 1.0;

  // Geometric square
  let mut writer = hound::WavWriter::create("geometric-square.wav", spec).unwrap();
  for s in geometric_square(rate, length) {
    writer.write_sample((s * amp * max) as i16).unwrap();
  }

  // Fourier series square
  writer = hound::WavWriter::create("fourier-square.wav", spec).unwrap();
  for s in fourier_square(rate, length) {
    writer.write_sample((s * amp * max) as i16).unwrap();
  }
}
