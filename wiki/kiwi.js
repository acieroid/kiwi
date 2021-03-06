function error(msg) {
    alert("Error: " + msg);
}

function handle_json_error(err) {
    try {
        error(JSON.parse(err.responseText).reason);
    } catch (e) {
        error("cannot parse response: " + e +
              ".\nResponse was: " + err.responseText);
    }
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
    $.post("/wiki/" + encodeURIComponent(name), function(data) {
        window.location.href = "/" + name + "/";
    }).fail(handle_json_error);
}

function addpage(wiki, name) {
    $.post("/wiki/" + wiki + "/" + encodeURIComponent(name), function(data) {
        window.location.href = "/" + wiki + "/" + name;
    }).fail(handle_json_error);
}

function save(wiki, page) {
    var content = $("#content").val();
    $.post("/wiki/" + wiki + "/" + page, content, function(data) {
        location.reload()
    }).fail(handle_json_error);
}

function edit(wiki, page) {
    $("#action").attr('onclick','').unbind('click');
    $("#action").html("save");
    $("#action").click(function () { save(wiki, page) });

    $("#main").html("<textarea name=\"content\" id=\"content\">");

    $.get("/wiki/" + wiki + "/" + page, function(data) {
        $("#content").val(data["content"]);
    }).fail(handle_json_error);
}
