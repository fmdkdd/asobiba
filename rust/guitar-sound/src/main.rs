extern crate hound;
extern crate rand;

use std::i16;
use std::collections::VecDeque;
use rand::distributions::{IndependentSample, Range};

// Toying with the Karplus-Strong algorithm
// See: http://introcs.cs.princeton.edu/java/assignments/guitar.html

type RingBuffer = VecDeque<f32>;

// Generate random samples
fn pluck(freq: u32, sample_rate: u32) -> RingBuffer {
  let mut rng = rand::thread_rng();
  let len = sample_rate / freq + 1;
  let range = Range::new(-0.5, 0.5);
  (0..len).map(|_| range.ind_sample(&mut rng)).collect()
}

// Average the randomness
fn vibrate(samples: &mut RingBuffer) {
  let a = samples.pop_front().unwrap();
  let b = samples[1];
  let decay_factor = 0.994;
  samples.push_back((a + b) / 2.0 * decay_factor);
}

// Root mean square, to get an idea of the amplitude of the whole buffer
fn rms(samples: &RingBuffer) -> f32 {
  samples.iter()
    .fold(0.0, |sum, s| sum + s * s)
    .sqrt()
}

fn main() {
  let note = 0;
  let freq = 440.0 * f32::powi(1.05956, note - 24);
  let sample_rate = 44100;
  let silence = 0.05;
  let mut samples = pluck(freq as u32, sample_rate);

  // Prepare WAV writer
  let spec = hound::WavSpec {
    channels: 1,
    sample_rate: sample_rate,
    bits_per_sample: 16,
    sample_format: hound::SampleFormat::Int,
  };
  let mut writer = hound::WavWriter::create("out.wav", spec).unwrap();

  // Then write a sample and vibrate, until silence
  let amplitude = i16::MAX as f32;
  while rms(&samples) > silence {
    writer.write_sample((samples[0] * amplitude) as i16).unwrap();
    vibrate(&mut samples);
  }
}
