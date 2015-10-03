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
            }
        });
    }

    var statusIcon = function(status){
        return (status === "created")? "fa-circle-o":
            (status === "running")? "fa-cog":
            (status === "stopping")? "fa-times":
            (status === "stopped")? "fa-times":
            (status === "completed")? "fa-check-circle":
            (status === "errored")? "fa-exclamation-triangle":
            "fa-question";
    }

    var formatDuration = function(duration){
        if(duration === null)
            return "-";
        else{
            var s = duration % 60;
            var m = Math.floor((duration/60)%60);
            var h = Math.floor(duration/(60*60));
            return ((h==0)?"":h+"hours")+((m==0)?"":" "+m+" minutes")+((s==0)?"":" "+s+" seconds");
        }
    }

    var formatDate = function(time){
        if(time === null)
            return "-";
        else{
            var z = function(date){return ("0" + date).slice(-2);}
            // Convert from universal to unix and make it ms based.
            var date = new Date((time-2208988800)*1000);
            return date.getFullYear()+"."+(date.getMonth()+1)+"."+date.getDate()+" "+z(date.getHours())+":"+z(date.getMinutes())+":"+z(date.getSeconds());
        }
    }

    var updateBuildStatus = function(project, hashes){
        $.ajax({
            url: "/api/autobuild/project/build",
            data: {"project": project, "build[]": hashes},
            dataType: "json",
            success: function(data){
                data = data.data;
                $.each(data, function(commit, data){
                    var build = $(".build[data-commit="+commit+"]");
                    $(".start time", build).text(formatDate(data.start));
                    $(".duration time", build).text(formatDuration(data.duration));
                    $(".status i", build).attr("class","fa "+statusIcon(data.status))
                        .text(data.status.toUpperCase());
                });
            }
        });
    }

    var updateLog = function(project, commit){
        $.ajax({
            url: "/api/autobuild/project/build/log",
            data: {"project": project, "build": commit},
            dataType: "json",
            success: function(data){
                data = data.data;
                $(".build[data-commit="+commit+"] #log code").text(data);
                Prism.highlightAll();
            }
        });
    }

    var updateAllStatus = function(){
        $(".project").each(function(){
            var commits = []
            $(".build",this).each(function(){commits.push($(this).data("commit"));});
            updateBuildStatus($(this).data("name"), commits);
        });
        $("article.build").each(function(){
            updateBuildStatus($(this).data("project"), [$(this).data("commit")]);
            updateLog($(this).data("project"), $(this).data("commit"));
        });
    }

    var update = function(){
        updateLoadInfo();
        updateAllStatus();
    }

    updateLoadInfo();
    // Takes a second to measure, so interval every 4 gives a total of 5.
    setInterval(update, 4000);
})
