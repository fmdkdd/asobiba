var o = Object.create(null);

Object.defineProperty(o, 'not_enum', {
  configurable: false,
  enumerable: false,
  value: 1,
  writable: false,
});

with (o) { not_enum; } //: 1
// Any own property is readable

with (o) { not_enum = 2; } //: 2
o.not_enum; //: 1
// Cannot write non-writeable

Object.defineProperty(o, 'writeable', {
  configurable: false,
  enumerable: false,
  value: 1,
  writable: true,
});

with (o) { writeable = 2; } //: 2
o.writeable; //: 2
// Writeable can erase content

var proto = Object.create(null);
Object.defineProperty(proto, 'inherited', {
  configurable: false,
  enumerable: false,
  writable: false,
  value: 1,
});

Object.setPrototypeOf(o, proto);
with (o) { inherited; } //: 1
// Inherited properties are visible

with (o) { inherited = 2; }
o.inherited; //: 1
proto.inherited; //: 1
// Non-writeable inherited properties do not change in the parent or child when assigned to.

o.inherited = 2;
o.inherited; //: 1
proto.inherited; //: 1
// Same behavior outside `with`.

Object.defineProperty(proto, 'inherited_writeable', {
  configurable: false,
  enumerable: false,
  writable: true,
  value: 1,
});

with (o) { inherited_writeable = 2; }
o.inherited_writeable; //: 2
proto.inherited_writeable; //: 1
// Writeable properties change on the object where the assignment took place, not where the property was found

o = {}; // Default object
