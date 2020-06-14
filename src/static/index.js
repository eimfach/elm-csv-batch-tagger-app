// pull in desired CSS/SASS files
require('./styles/main.less')
const uikit = require('uikit')
const icons = require('uikit/dist/js/uikit-icons')

uikit.use(icons)

// inject bundled Elm app into div#main
var Elm = require('../elm/Main')

var storedState = localStorage.getItem('store');
var startingState = storedState ? JSON.parse(storedState) : null;

var app = Elm.Elm.Main.init({
  node: document.getElementById('main'),
  flags: startingState
});

app.ports.getLocale.send(navigator.language ? navigator.language : "en-EN");

app.ports.setStorage.subscribe(function(state) {
  localStorage.setItem('store', JSON.stringify(state));
});

app.ports.deleteStorage.subscribe(function() {
  localStorage.removeItem('store');
})
