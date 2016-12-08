var initialized = false;

function init () {
	if (!initialized) {
		initialized = true;
		var elem = document.getElementById("model");
		xhttp = new XMLHttpRequest();
		xhttp.onreadystatechange = function() {
			if (this.readyState == 4 && this.status == 200) {
				elem.innerHTML = this.responseText;
			}
		}
		xhttp.open("GET","example.uml")
		xhttp.send();
	}
};

document.addEventListener("readystatechange",function(doc,ev) {
	if (document.readyState == "interactive") init()
});