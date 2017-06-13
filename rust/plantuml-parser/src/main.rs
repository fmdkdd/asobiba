#[macro_use] extern crate nom;

use nom::{alphanumeric, multispace, space};

type ClassDiagram = Vec<Decl>;

#[derive(Debug)]
enum Decl {
  Package { name: String, body: Vec<Decl> },
  Node { ty: NodeType, name: String, fields: Vec<Field> },
  Link { ty: LinkType, source: String, target: String },
}

#[derive(Debug)]
enum Field {
  Attribute { visibility: Visibility, name: String, ty: Option<String> },
  Method { visibility: Visibility, name: String, ty: Option<String>, args: Vec<Arg> },
}

#[derive(Debug)]
struct Arg {
  name: String,
  ty: Option<String>,
}

#[derive(Debug)]
enum Visibility {
  Public,
  Private,
  Protected,
}

#[derive(Debug)]
enum NodeType {
  AbstractClass,
  Class,
  Interface,
}

#[derive(Debug)]
enum LinkType {
  Association,
  Inheritance,
}

named!(parse_class_diagram<&str, ClassDiagram>,
       alt_complete!(many0!(parse_link)    |
                     many0!(parse_package) |
                     many0!(parse_node)));

named!(parse_link<&str, Decl>, do_parse!(
  source: alphanumeric >>
  opt!(space) >>
  ty: parse_arrow >>
  opt!(space) >>
  target: alphanumeric >>
  opt!(multispace) >>

  (Decl::Link { ty, source: source.into(), target: target.into() })
));

named!(parse_arrow<&str, LinkType>,
       alt_complete!(map!(tag!("-->"), |_| LinkType::Association) |
                     map!(tag!("--|>"), |_| LinkType::Inheritance)));

named!(parse_package<&str, Decl>, do_parse!(
  tag!("package") >>
  opt!(space) >>
  name: alphanumeric >>
  opt!(space) >>
  char!('{') >>
  opt!(multispace) >>
  body: many0!(parse_node) >>
  opt!(multispace) >>
  char!('}') >>

  (Decl::Package { name: name.into(), body, })
));

named!(parse_node<&str, Decl>, do_parse!(
  ty: parse_node_type >>
  opt!(space) >>
  name: alphanumeric >>
  opt!(space) >>
  char!('{') >>
  opt!(multispace) >>
  fields: separated_list!(char!('\n'), delimited!(opt!(space), parse_field, opt!(space))) >>
  opt!(multispace) >>
  char!('}') >>

  (Decl::Node { ty, name: name.into(), fields, })
));

named!(parse_node_type<&str, NodeType>, alt_complete!(
  map!(tag!("class"), |_| NodeType::Class)                  |
  map!(tag!("abstract class"), |_| NodeType::AbstractClass) |
  map!(tag!("interface"), |_| NodeType::Interface)));

named!(parse_field<&str, Field>, alt_complete!(
  parse_method |                // order matters here
  parse_attribute));

named!(parse_attribute<&str, Field>, do_parse!(
  visibility: parse_visibility >>
  name: alphanumeric >>
  ty: opt!(complete!(parse_type)) >>

  (Field::Attribute { visibility, name: name.into(), ty, })
));

named!(parse_method<&str, Field>, do_parse!(
  visibility: parse_visibility >>
  name: alphanumeric >>
  args: delimited!(char!('('), separated_list!(char!(','), parse_method_arg), char!(')')) >>
  ty: opt!(complete!(parse_type)) >>

  (Field::Method { visibility, name: name.into(), args: Vec::new(), ty, })
));

named!(parse_method_arg<&str, Arg>, do_parse!(
  name: alphanumeric >>
  ty: opt!(complete!(parse_type)) >>

  (Arg { name: name.into(), ty, })
));

named!(parse_type<&str, String>, do_parse!(
  opt!(space) >>
  char!(':') >>
  opt!(space) >>
  ty: alphanumeric >>
  (ty.into())
));

named!(parse_visibility<&str, Visibility>, alt_complete!(
  map!(char!('-'), |_| Visibility::Private)   |
  map!(char!('+'), |_| Visibility::Public)    |
  map!(char!('~'), |_| Visibility::Protected)
));

#[test]
fn test_node_decl() {
  let input = "class Foo {
  +attribute: String
  ~method(a: int): Object
}";

  println!("{:?}", parse_node_type(input));
}

fn main() {
  {
    let input = "class Foo {
  +attribute: String
  ~method(a: int): Object
}";
    println!("{:?}", parse_node(input));
  }

  {
    let input = "+attribute : foo";
    println!("{:?}", parse_attribute(input));
  }

  {
    let input = "Foo --> Bar
Bar --|> Baz
";

    println!("{:?}", parse_class_diagram(input));
  }

  {
    let input = "package A {
  class Foo {
    +attribute: String
    ~method(a: int): Object
  }
}";

    println!("{:?}", parse_package(input));
  }

  {
    let input = "package A {
  class Foo {
    +attribute: String
    ~method(a: int): Object
  }
}

Foo --> Bar
Bar --|> Baz
";

    println!("{:?}", parse_class_diagram(input));
  }


}
