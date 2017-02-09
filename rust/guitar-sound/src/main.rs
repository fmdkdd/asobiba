extern crate hound;
extern crate rand;

use std::i16;
use std::collections::VecDeque;
use rand::distributions::{IndependentSample, Range};

// Toying with the Karplus-Strong algorithm
// See: http://introcs.cs.princeton.edu/java/assignments/guitar.html

type RingBuffer = VecDeque<f32>;

// Generate random samples
fn pluck(freq: u16, sample_rate: u16) -> RingBuffer {
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

fn main() {
  let note = 0;
  let freq = 440.0 * f32::powi(1.05956, note - 24);
  let sample_rate = 44100;
  let mut samples = pluck(freq as u16, sample_rate);

  // Write the samples to WAV
  let spec = hound::WavSpec {
    channels: 1,
    sample_rate: 44100,
    bits_per_sample: 16,
    sample_format: hound::SampleFormat::Int,
  };

  let mut writer = hound::WavWriter::create("out.wav", spec).unwrap();

  for s in samples.iter() {
    let amplitude = i16::MAX as f32;
    writer.write_sample((s * amplitude) as i16).unwrap();
  }

  // Then vibrate, and write again, a bunch of times
  for _ in 0..400 {
    for _ in 0..samples.len() {
      vibrate(&mut samples);
    }

    for s in samples.iter() {
      let amplitude = i16::MAX as f32;
      writer.write_sample((s * amplitude) as i16).unwrap();
    }
  }

}
