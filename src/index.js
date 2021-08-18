'use strict';

require('./index.html');
const {Elm} = require('./Main.elm');

console.log('Live reload 7')

Elm.Main.init({node: document.getElementById('main')});