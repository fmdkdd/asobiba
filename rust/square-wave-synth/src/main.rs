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

// Sawtooth made from subtracting even harmonics from odd harmonics in the
// Fourier series
fn fourier_sawtooth(rate: f32, length: f32, phase_shift: f32) -> Vec<f32> {
  // How many samples do we need?
  let samples = length * (SAMPLE_RATE as f32);

  // How many periods per sample?
  let ratio = rate / (SAMPLE_RATE as f32);

  (0..(samples as u32)).map(|s| {
    // Sum the Fourier series until the Nyquist barrier
    (1u32..)
      .map(|h| h as f32)
    // Limit to frequencies below Nyquist
      .take_while(|h| (h * rate) < ((SAMPLE_RATE as f32) / 2.0))
      .map(|h| {
        // t in [0,1] is a time index into a wave period
        let t = ratio * (s as f32);
        let s = f32::sin(2.0 * f32::consts::PI * h * (t + phase_shift)) / h;
        if (h as u32) % 2 == 0 { -s }
        else { s }
      }).sum()
  }).collect()
}

// Square wave from subtracting two sawtooth waves
fn sawtooth_square(rate: f32, length: f32) -> Vec<f32> {
  let saw = fourier_sawtooth(rate, length, 0.0);

  // How many samples to offset?  For a 50/50 square, we need to offset by half
  // a period.
  let pulse_width = 1.0 / 2.0;
  let offset = ((SAMPLE_RATE as f32) / rate * pulse_width) as usize;

  // Another way to do that is to create two sawtooth waves and use the
  // phase_shift parameter

  saw.iter().zip(saw.iter().skip(offset)).map(|(a,b)| a - b).collect()
}

// Fourier square wave with amplitude modulated by a sawtooth, trying to
// replicate the waveform shape of a DMG pulse channel.
fn square_sawtooth_amp(rate: f32, length: f32) -> Vec<f32> {
  let square = fourier_square(rate, length);
  let saw = fourier_sawtooth(rate, length, 0.0);

  let ratio = 0.125;

  square.iter().zip(saw.iter())
    .map(|(a, b)| {
      if *b > 0.0 { (a - (b * ratio)) / (1.0 + ratio) }
      else        { (a + (b * ratio)) / (1.0 + ratio) }
    }).collect()
}

fn main() {
  // Prepare the WAV writer
  let spec = hound::WavSpec {
    channels: 1,
    sample_rate: SAMPLE_RATE,
    bits_per_sample: 8,
    sample_format: hound::SampleFormat::Int,
  };
  let max = 127 as f32;
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

  // Fourier series square made from two sawtooth
  writer = hound::WavWriter::create("fourier-sawtooth-square.wav", spec).unwrap();
  for s in sawtooth_square(rate, length) {
    writer.write_sample((s * amp * max) as i16).unwrap();
  }

  // Square wave with amplitude modulated by a sawtooth
  writer = hound::WavWriter::create("fourier-square-sawtooth-amp.wav", spec).unwrap();
  for s in square_sawtooth_amp(rate, length) {
    writer.write_sample((s * amp * max) as i16).unwrap();
  }
}
