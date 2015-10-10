var Autobuild = function(){
    var self = this;

    self.log = function(){
        var args = $.extend([], arguments);
        args.unshift("[Autobuild]");
        console.log.apply(console, args);
        return true;
    }

    self.val = function(key, val){
        return (val === undefined)?
            JSON.parse(localStorage.getItem(key)):
            localStorage.setItem(key,JSON.stringify(val));
    }
    
    self.statusIcon = function(status){
        return (status === "created")? "fa-circle-o":
            (status === "scheduled")? "fa-ellipsis-h":
            (status === "running")? "fa-cog":
            (status === "stopping")? "fa-ellipsis-h":
            (status === "stopped")? "fa-times":
            (status === "completed")? "fa-check-circle":
            (status === "errored")? "fa-exclamation-triangle":
            "fa-question";
    }

    self.formatDuration = function(duration){
        if(duration === null)
            return "-";
        else{
            var s = duration % 60;
            var m = Math.floor((duration/60)%60);
            var h = Math.floor(duration/(60*60));
            return ((h==0)?"":h+"hours")+((m==0)?"":" "+m+" minutes")+(((s==0)&&(m!=0||h!=0))?"":" "+s+" seconds");
        }
    }

    self.formatDate = function(time){
        if(time === null)
            return "-";
        else{
            var z = function(date){return ("0" + date).slice(-2);}
            // Convert from universal to unix and make it ms based.
            var date = new Date((time-2208988800)*1000);
            return date.getFullYear()+"."+(date.getMonth()+1)+"."+date.getDate()+" "+z(date.getHours())+":"+z(date.getMinutes())+":"+z(date.getSeconds());
        }
    }

    self.updateLoadInfo = function(){
        $.ajax({
            url: "/api/autobuild/system/load",
            dataType: "json",
            success: function(data){
                data = data.data;
                $(".system-load .cpu .percentage").text(Number(data.cpuUsage.toFixed(2))+"");
                $(".system-load .cpu .bar div").css("width",data.cpuUsage+"%");
                $(".system-load .ram .percentage").text(Number(data.ramUsage.toFixed(2))+"");
                $(".system-load .ram .bar div").css("width",data.ramUsage+"%");
                $(".system-load .mem .percentage").text(Number(data.memUsage.toFixed(2))+"");
                $(".system-load .mem .bar div").css("width",data.memUsage+"%");
            }
        });
    }

    self.updateBuildStatus = function(project, hashes){
        $.ajax({
            url: "/api/autobuild/project/build",
            data: {"project": project, "build[]": hashes},
            dataType: "json",
            success: function(data){
                data = data.data;
                $.each(data, function(commit, data){
                    var $build = $(".build[data-project="+project+"][data-commit="+commit+"]");
                    $(".start time", $build).text(self.formatDate(data.start));
                    $(".duration time", $build).text(self.formatDuration(data.duration));
                    if($build.attr("data-status") !== data.status){
                        self.notify(project+" build "+commit+" changed status to "+data.status+".");
                        $build.attr("data-status", data.status);
                        // Cheapo way of doing it.
                        $(".status",$build).attr("class","status "+data.status);
                        $(".status i", $build).attr("class","fa "+self.statusIcon(data.status))
                            .text(data.status.toUpperCase());
                    }
                });
            }
        });
    }

    var logPosition = 0;
    self.updateLog = function(project, commit){
        $.ajax({
            url: "/api/autobuild/project/build/log",
            data: {"project": project, "build": commit, "file-position": logPosition},
            dataType: "json",
            success: function(data){
                data = data.data;
                if(data.text === null)return;
                
                var $log = $(".build[data-commit="+commit+"] #log code");
                if(logPosition == 0){
                    $log.text(data.text);
                }else{
                    $log.text($log.text()+data.text);
                }
                Prism.highlightElement($log[0]);
                logPosition = data.position;
            }
        });
    }

    self.updateAllStatus = function(){
        $(".project").each(function(){
            var commits = []
            $(".build",this).each(function(){commits.push($(this).attr("data-commit"));});
            self.updateBuildStatus($(this).attr("data-name"), commits);
        });
        $("article.build").each(function(){
            self.updateBuildStatus($(this).attr("data-project"), [$(this).attr("data-commit")]);
            self.updateLog($(this).attr("data-project"), $(this).attr("data-commit"));
        });
    }

    self.update = function(){
        self.log("Fetching updates..");
        self.updateLoadInfo();
        self.updateAllStatus();
    }

    self.initUpdate = function(){
        self.updateLoadInfo();
        setInterval(self.update, 4000);
    }

    self.initConfirm = function(){
        $("a.confirm").each(function(){
            var self = this;
            $(self).click(function(){
                var title = $(self).attr("title")[0].toLowerCase() + $(self).attr("title").slice(1);
                return confirm("Are you sure you want to "+title+"?");
            });
        });
    }

    self.initRecipe = function(){
        $(".recipe").each(function(){
            var mirror = CodeMirror.fromTextArea($("textarea",this)[0], {
                mode: "commonlisp",
                keyMap: "emacs",
                lineNumbers: true,
                autoCloseBrackets: true,
                matchBrackets: true,
                extraKeys: {
                    Enter: function(cm){
                        cm.execCommand("newlineAndIndent");
                    }
                }
            });
        });
    }

    self.initLog = function(){
        $("#log code").each(function(){
            Prism.highlightElement(this);
        });
    }

    var useNotifications = false;
    self.maybeEnableNotifications = function(cont){
        if("Notification" in window){
            if (Notification.permission === "granted") {
                self.log("Found notification permission.");
                cont();
                return true;
            }else{
                Notification.requestPermission(function(permission){
                    if (permission === "granted") {
                        self.log("Got notification permission.");
                        cont();
                        return true;
                    }else{
                        self.log("Notification permission denied.");
                    }
                });
            }
        }
        return false;
    }

    self.setUseNotifications = function(bool){
        if(bool){
            self.maybeEnableNotifications(function(){
                $(".global-options .notifications").addClass("active");
                useNotifications = true;
            });
        }else{
            useNotifications = false;
            $(".global-options .notifications").removeClass("active");
        }
        self.val("notifications",bool);
    }

    self.initNotifications = function(){
        $(".global-options .notifications").click(function(){
            self.setUseNotifications(!useNotifications);
        });
        self.setUseNotifications(self.val("notifications"));
    }

    self.notify = function(message){
        if(!useNotifications)return;
        
        self.log("Notifying with",message);
        return new Notification("Autobuild Update",{
            body: message});
    }

    self.init = function(){
        self.log("Initializing.");
        self.initConfirm();
        self.initRecipe();
        self.initNotifications();
        self.initLog();
        self.initUpdate();
    }

    return self;
}

autobuild = new Autobuild();

$(function(){
    autobuild.init();
})
