// Hace un loop para acceder a la tabla de todos los llamados publicados, página por página, usando PhantomJS
// y genera archivos .txt con el código html con las tablas de llamados laborales
// El portal tardaba mas de un minuto en cargar cada pagina, asi que use un timer de dos minutos para esperar
// que cambiara de pagina

var fs = require('fs');
var webpage = require('webpage');
var page = webpage.create();
page.viewportSize = { width: 1366, height: 768 };
page.onConsoleMessage = function(msg) {
    console.log(msg);
};

function readNwrite(j) {
	var coso = page.evaluate(function() {
  		return document.querySelector("#Grid1ContainerTbl").innerHTML;
	});
	fs.write(j+".txt", coso, 'w'); 
}

page.open('https://www.uruguayconcursa.gub.uy/Portal/servlet/com.si.recsel.inicio', function(status) {
	page.includeJs('http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js', function() {
		page.evaluate(function() {
			var sel = document.querySelector("#vFLLAESTWEB");
			sel.selectedIndex = 0;
			document.querySelector(".ButtonAcceptDrag").click();
		});
		function scrape() {
			var i = 0;
			var scraping = setInterval(processor, 120000);
			function processor() {
				if (i===0) {
					i++;
				} else {
					readNwrite(i);
					page.evaluate(function() {
						document.querySelector("#NEXT").click();
					});
					i++;
				}
			}
		}
		scrape();
		phantomjs.exit();
	});
});
