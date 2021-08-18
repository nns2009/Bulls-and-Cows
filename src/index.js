'use strict';

import './second.js'

//require('./index.html');
const {Elm} = require('./Main.elm');

console.log('index 23')

Elm.Main.init({node: document.getElementById('main')});


if (module.hot) {
	module.hot.accept('./second.js', () => {
		console.log('Change detected');
	});
}