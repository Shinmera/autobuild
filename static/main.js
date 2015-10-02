$(function(){
    $("a.confirm").each(function(){
        var self = this;
        $(self).click(function(){
            var title = $(self).attr("title")[0].toLowerCase() + $(self).attr("title").slice(1);
            return confirm("Are you sure you want to "+title+"?");
        });
    });

    var updateLoadInfo = function(){
        console.log("Fetching load info..");
        $.ajax({
            url: "/api/autobuild/system/load",
            dataType: "json",
            success: function(data){
                data = data.data;
                $("#system-load .cpu .percentage").text(Number(data.cpuUsage.toFixed(2))+"");
                $("#system-load .cpu .bar div").css("width",data.cpuUsage+"%");
                $("#system-load .ram .percentage").text(Number(data.ramUsage.toFixed(2))+"");
                $("#system-load .ram .bar div").css("width",data.ramUsage+"%");
                $("#system-load .mem .percentage").text(Number(data.memUsage.toFixed(2))+"");
                $("#system-load .mem .bar div").css("width",data.memUsage+"%");
            },
        });
    }

    updateLoadInfo();
    setInterval(updateLoadInfo, 5000);
})
