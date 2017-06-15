use std::env;

struct Ctxt {
  pc: u16,
  mem: [u8; 0x1000],
}

fn nop(ctxt: &mut Ctxt) { println!("nop"); }
fn add(ctxt: &mut Ctxt) { println!("add"); }
fn sub(ctxt: &mut Ctxt) { println!("sub"); }

fn main() {
  let args : Vec<_> = env::args().collect();
  let opcode = args[0].parse::<u16>().unwrap();
  let mut op_table : [fn(&mut Ctxt); 0x10000] = [nop; 0x10000];

  let mut c = Ctxt { pc: 0, mem: [0; 0x1000] };

  op_table[12] = add;
  op_table[31] = sub;

  op_table[(opcode as usize)](&mut c)
}
