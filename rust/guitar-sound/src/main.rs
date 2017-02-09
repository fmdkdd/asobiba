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

fn note_freq(note: i32) -> f32 {
  440.0 * f32::powi(1.05956, note - 24)
}

const SAMPLE_RATE: u32 = 44100;
const SILENCE : f32 = 0.05;

struct GuitarString {
  buf: RingBuffer,
}

impl GuitarString {
  fn new(note: i32) -> Self {
    GuitarString {
      buf: pluck(note_freq(note) as u32, SAMPLE_RATE)
    }
  }

  fn tick(&mut self) -> f32 {
    let r = self.buf[0];
    vibrate(&mut self.buf);
    r
  }

  fn is_vibrating(&self) -> bool {
    rms(&self.buf) > SILENCE
  }
}

fn main() {
  let mut strings = vec![GuitarString::new(0),
                         GuitarString::new(3),
                         GuitarString::new(7)];

  // Prepare WAV writer
  let spec = hound::WavSpec {
    channels: 1,
    sample_rate: SAMPLE_RATE,
    bits_per_sample: 16,
    sample_format: hound::SampleFormat::Int,
  };
  let mut writer = hound::WavWriter::create("out.wav", spec).unwrap();

  // Then write a sample and vibrate, until silence
  let amplitude = i16::MAX as f32;
  while strings.iter().any(|s| s.is_vibrating()) {
    let sample = strings.iter_mut().fold(0.0, |sum, s| sum + s.tick());
    writer.write_sample((sample * amplitude) as i16).unwrap();
  }
}
