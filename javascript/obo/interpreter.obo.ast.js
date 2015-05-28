object('interpreter',
       [
         method('interpret-node',
                func('node',
                     apply(
                         methodCall(
                           methodCall(
                             identifier('self'),
                             identifier('rules')),
                           eval(methodCall(
                             identifier('node'),
                             identifier('type')))),
                         identifier('node'))))),

         method('rules',
                object('rules',
                       [
                         method('identifier',
                               func('node',
                                    methodCall(
                                      methodCall(
                                        methodCall(
                                          identifier('self'),
                                          identifier('runtime')),
                                        identifier('identifiers')),
                                      eval(methodCall(
                                        identifier('node'),
                                        identifier('value')))))),

                         method('object',
                               func('node',
                                    apply(
                                      func('obj',
                                           forIn('name', 'm',
                                                 methodCall(
                                                   identifier('node'),
                                                   identifier('methods')),
                                                 methodUpdate(
                                                   methodCall(
                                                     methodCall(
                                                       identifier('obj'),
                                                       identifier('methods')),
                                                     eval(identifier('name'))),
                                                   apply(
                                                     methodCall(
                                                       identifier('self'),
                                                       identifier('interpret-node')),
                                                     identifier('m'))
                                                 )
                                          )),
                                      object(null, [])))),

                         method('method',
                               func('node',
                                   )),

                         method('method-call',
                               func('node',
                                   )),
                       ]
                      )),

         method('runtime',
                object('runtime',
                       [method('identifiers', object('identifiers', []))]))
       ])

interpreter {
  interpret-node (node) ->
    self.rules[node.type](node)

  rules {
    identifier (node) ->
      return self.runtime.identifiers[node.value];

    object (node) ->
      var obj = {}
      for-in (name m) node.methods
        obj.methods[name] = self.interpret-node(m)
      return obj

    method (node) ->
      return {
        name: node.name
        body: node.body
        invoke (withSelf) ->
          self.runtime.identifiers.self = self
          return self.interpret-node(self.body)
      }

    method-call (node) ->
      var r = self.interpret-node(node.receiver)
      var m = node.method-name.value
      var v = node.body

      r[m] = v
      return r
  }

  runtime {
    identifiers {}
  }
}
