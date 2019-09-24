// pull in desired CSS/SASS files
require('./styles/main.less')
const uikit = require('uikit')
const icons = require('uikit/dist/js/uikit-icons')

/*if (process.browser === undefined) {
  const { ipcRenderer } = require('electron')
  ipcRenderer.on('asynchronous-message', (event, arg) => {
    console.log(arg) // prints "ping"
    event.reply('asynchronous-reply', 'pong')
  })
  ipcRenderer.send('asynchronous-message', 'ping')
}*/


uikit.use(icons)
// uikit.notification('Hello world Uikit.', 200000);

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

app.ports.fileSelected.subscribe(function (id) {
  var node = document.getElementById(id)
  if (node === null) {
    return
  }

  // If your file upload field allows multiple files, you might
  // want to consider turning this into a `for` loop.
  var file = node.files[0]
  var reader = new FileReader()

  // FileReader API is event based. Once a file is selected
  // it fires events. We hook into the `onload` event for our reader.
  reader.onload = function (event) {
    // The event carries the `target`. The `target` is the file
    // that was selected. The result is base64 encoded contents of the file.
    var base64encoded = event.target.result
    // We build up the `ImagePortData` object here that will be passed to our Elm
    // runtime through the `fileContentRead` subscription.
    var portData = {
      contents: base64encoded.split(',')[1],
      filename: file.name
    }

    // We call the `fileContentRead` port with the file data
    // which will be sent to our Elm runtime via Subscriptions.
    app.ports.fileContentRead.send(portData)
  }

  // Connect our FileReader with the file that was selected in our `input` node.
  reader.readAsDataURL(file)
})
