self.onmessage = function(event) {
    var xhr = new XMLHttpRequest();
    var url = event.data;

    xhr.open('GET', url, false);
    xhr.responseType = 'arraybuffer';
    xhr.send();
    postMessage(new Uint8Array(xhr.response));
};

