
console.log('launchElm loading ...');

const { Elm } = require('./Main.elm');

const storedData = localStorage.getItem('model');
const flags = storedData ? JSON.parse(storedData) : null;

const app = Elm.Main.init({
	node: document.getElementById('main'),
	flags,
});

app.ports.save.subscribe(model => {
	//console.info(model);
	localStorage.setItem('model', JSON.stringify(model));
});
