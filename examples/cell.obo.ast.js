object('cell', [
  method('contents', object(null, [])),
  method('set',
         func('n',
              methodUpdate(
                identifier('self'),
                identifier('contents'),
                identifier('n')
              ))
         ),
  method('run',
         methodCall(
           apply(
             methodCall(
               identifier('self'),
               identifier('set')),
             object(null, [
               method('hello',
                      object(null, []))
             ])),
           identifier('contents')))
]);
