// Inter-type sharing from "Organizing Programs Without Classes", Ungar,
// Chambers, Chang and Hölzle, 1991.

var SetTrait = {
  copy() {
    return {
      __proto__: this,
      items: Array.from(this.items)
    }
  },

  add(x) {
    if (this.items.indexOf(x) === -1)
      this.items.push(x)
    return this
  },

  remove(x) {
    var i = this.items.indexOf(x)
    if (i > -1)
      this.items.splice(i, 1)
    return this
  },

  get length() {
    return this.items.length
  },

  toString() {
    return `{${this.items.join(',')}}`
  },
}

var SetPrototype = {
  __proto__: SetTrait,
  items: [],
}

var aSet = SetPrototype.copy()
aSet.toString() //: "{}"
aSet.add(1).add(2).toString() //: "{1,2}"
aSet.length //: 2
var bSet = SetPrototype.copy()
bSet.toString() //: "{}"

var cSet = aSet.copy()
cSet.toString() //: "{1,2}"
cSet.add(3).toString() //: "{1,2,3}"
aSet.toString() //: "{1,2}"
bSet.toString() //: "{}"
