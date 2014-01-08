function error(msg) {
    alert("Error: " + msg);
}

function create() {
    var name = $("#name").val();
    $.post("/wiki/" + name, function(data) {
        window.location.href = name + "/";
    });
}

function save(wiki, page) {
    var content = $("#content").val();
    $.post("/wiki/" + wiki + "/" + page, content, function(data) {
        location.reload()
    });
}

function edit(wiki, page) {
    $("#action").attr('onclick','').unbind('click');
    $("#action").html("save");
    $("#action").click(function () { save(wiki, page) });

    $("#main").html("<textarea name=\"content\" id=\"content\">");

    $.get("/wiki/" + wiki + "/" + page, function(data) {
        $("#content").val(data["content"]);
    }).fail(function() {
        error("cannot access API");
    });
}
