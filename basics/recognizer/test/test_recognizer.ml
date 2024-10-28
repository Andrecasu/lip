open Recognizer

let%test _ = belongsTo ['0'] = [true; true; false; false; false]
let%test _ = belongsTo ['0'; '0'; '1'; '0'] = [true; false; true; false; false]
let%test _ = belongsTo [] = [false; true; false; false; false]
let%test _ = belongsTo ['1'; '0'; '0'] = [true; false; false; false; false]
