object('obo', [
  method('empty', object(null, [])),
  method('run',
         methodCall(
           identifier('self'),
           identifier('empty')
         ))
]);
