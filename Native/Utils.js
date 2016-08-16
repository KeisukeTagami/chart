

var _KeisukeTagami$elm_chart$Native_Utils = function() {

    function log(x) {
        console.log(x);
        return x;
    }

    var svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    svg.setAttributeNS(null, 'version', '1.1');
    svg.setAttribute("xmlns", "http://www.w3.org/2000/svg");
    svg.setAttribute("xmlns:xlink", "http://www.w3.org/1999/xlink");
    svg.setAttribute("width", "0");
    svg.setAttribute("height", "0");
    svg.setAttribute("style", "position: fixed;");

    function build(x) {
        switch( x.type ) {
            case "node":
                var e = document.createElementNS(x.namespace, x.tag);
                for( var i in x.children ) {
                    var c = build(x.children[i]);
                    e.appendChild(c);
                }
                return e;
                break;
            case "text":
                return document.createTextNode(x.text);
                break;
        }
        return null;
    }


    function width(x){
        return size(x)._0;
    }

    function height(x){
        return size(x)._1;
    }

    function size(x) {
        var text = document.createElementNS("http://www.w3.org/2000/svg", "text");
        var txtnode = document.createTextNode("100");

        var node = build(x);
        document.body.appendChild(svg);
        svg.appendChild(node);
        var bbox = node.getBBox();
        console.log(x);
        console.log(bbox);
        svg.removeChild(node);
        document.body.removeChild(svg);
        return _elm_lang$core$Native_Utils.Tuple2(bbox.width, bbox.height);
    }

    return {
        log: log,
        width: width,
        height: height,
        size: size
    };
}();
