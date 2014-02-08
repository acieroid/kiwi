function error(msg) {
    // TODO: neatly display this message
    alert("Error: " + msg);
}

function create() {
    $("#dialog").html("\
<form>\
<label for=\"name\">Wiki name: </label>\
<input type=\"input\" id=\"name\"/><br/>\
<input type=\"submit\" id=\"submit\" value=\"create!\"/>\
</form>");
    $("#dialog").show();
    $("#submit").click(function () {
        addwiki($("#name").val());
        $("#dialog").html("");
        $("#dialog").hide();
    });
}

function newpage(wiki) {
    $("#dialog").html("\
<form>\
<label for=\"name\">Page name:</label>\
<input type=\"input\" id=\"name\"/><br/>\
<input type=\"submit\" id=\"submit\" value=\"create!\"/>\
</form>");
    $("#dialog").show();
    $("#submit").click(function () {
        addpage(wiki, $("#name").val());
        $("#dialog").html("");
        $("#dialog").hide();
    });
}

function addwiki(name) {
    $.post("/wiki/" + name, function(data) {
        window.location.href = "/" + name + "/";
    }).fail(function(data) {
        error(data.responseText);
    });
}

function addpage(wiki, name) {
    $.post("/wiki/" + wiki + "/" + name, function(data) {
        window.location.href = "/" + wiki + "/" + name;
    }).fail(function(data) {
        error(data.responseText);
    });
}

function save(wiki, page) {
    var content = $("#content").val();
    $.post("/wiki/" + wiki + "/" + page, content, function(data) {
        location.reload()
    }).fail(function(data) {
        error(data.responseText);
    });
}

function edit(wiki, page) {
    $("#action").attr('onclick','').unbind('click');
    $("#action").html("save");
    $("#action").click(function () { save(wiki, page) });

    $("#main").html("<textarea name=\"content\" id=\"content\">");

    $.get("/wiki/" + wiki + "/" + page, function(data) {
        $("#content").val(data["content"]);
    }).fail(function(data) {
        error(data.responseText);
    });
}
