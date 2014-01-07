function error(msg) {
    alert("Error: " + msg);
}

function save(wiki, page) {
    var content = $("#content").val();
    alert(content);
    $.post("/api/wiki/" + wiki + "/" + page, content, function(data) {
        alert(data["result"]);
    });

    $("#action").attr('onclick','').unbind('click');
    $("#action").html("edit");
    $("#action").click(function () { edit(wiki, page) });
}

function edit(wiki, page) {
    $("#action").attr('onclick','').unbind('click');
    $("#action").html("save");
    $("#action").click(function () { save(wiki, page) });

    $("#main").html("<textarea name=\"content\" id=\"content\">");

    $.get("/api/wiki/" + wiki + "/" + page, function(data) {
        $("#content").val(data["content"]);
    }).fail(function() {
        error("cannot access API");
    });
}
