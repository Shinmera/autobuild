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
            (status === "stopped")? "fa-dot-circle-o":
            (status === "completed")? "fa-check-circle":
            (status === "errored")? "fa-exclamation-triangle":
            "fa-question";
    }

    self.formatDuration = function(duration){
        if(duration === null)
            return "-";
        else{
            var maybes = function(s){return (1<s)? "s": "";}
            var s = duration % 60;
            var m = Math.floor((duration/60)%60);
            var h = Math.floor(duration/(60*60));
            return ((h==0)?"":h+" hour"+maybes(h))+
                   ((m==0)?"":" "+m+" minute"+maybes(m))+
                   (((s==0)&&(m!=0||h!=0))?"":" "+s+" second"+maybes(s));
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

    self.api = function(url,data,success){
        $.ajax({
            url: "/api/autobuild/"+url,
            data: data,
            dataType: "json",
            success: function(data){
                if(data.status !== 200){
                    self.log("[API] Request to",url,"with",data,"failed:",data.message);
                    return null;
                }
                success(data.data);
            },
            error: function(){
                self.log("[API][Error] Request to",url,"with",data,"failed.");
            }
        });
    }

    self.updateLoadInfo = function(){
        self.api("system/load", {},
                 function(data){
                     $(".system-load .cpu .percentage").text(Number(data.cpuUsage.toFixed(2))+"");
                     $(".system-load .cpu .bar div").css("width",data.cpuUsage+"%");
                     $(".system-load .ram .percentage").text(Number(data.ramUsage.toFixed(2))+"");
                     $(".system-load .ram .bar div").css("width",data.ramUsage+"%");
                     $(".system-load .mem .percentage").text(Number(data.memUsage.toFixed(2))+"");
                     $(".system-load .mem .bar div").css("width",data.memUsage+"%");
                 });
    }

    self.updateBuildStatus = function(project, hashes){
        if(hashes.length == 0)return;
        self.api("project/build", {"project": project, "build[]": hashes},
                 function(data){
                     $.each(data, function(commit, data){
                         var $build = $(".build[data-project="+project+"][data-commit="+commit+"]");
                         if($build[0].tagName == "article"){
                             $(".build+dd .start", $build).text(self.formatDate(data.start));
                             $(".build+dd .end", $build).text(self.formatDate(data.end));
                             $(".build+dd .duration", $build).text(self.formatDuration(data.duration));
                             $.each(data.stages, function(name, data){
                                 $(".stage."+name+"+dd .duration").text(self.formatDuration(data.duration));
                             });
                         }else{
                             $(".start", $build).text(self.formatDate(data.start));
                             $(".duration", $build).text(self.formatDuration(data.duration));
                         }                             
                         if($build.attr("data-status") !== data.status){
                             self.notify(project+" build "+commit+" changed status to "+data.status+".");
                             $build.attr("data-status", data.status);
                             // Cheapo way of doing it.
                             $(".status",$build).attr("class","status "+data.status);
                             $(".status i", $build).attr("class","fa "+self.statusIcon(data.status))
                                 .text(data.status.toUpperCase());
                         }
                     });
                 });
    }

    var logPosition = parseInt($(".build .log input[name=log-position]").val() || "0");
    self.updateLog = function(project, commit){
        self.api("project/build/log", {"project": project, "build": commit, "file-position": logPosition},
                 function(data){
                     if(data.text === null || data.text.length == 0)return;
                     
                     var mirror = $(".build[data-commit="+commit+"] .log").data("mirror");
                     if(logPosition == 0){
                         data.text = data.text.substr(mirror.getDoc().getValue().length);
                     }
                     mirror.getDoc().replaceRange(data.text, {line: Infinity});
                     mirror.getDoc().setCursor({line: Infinity});
                     logPosition = data.position;
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
            $(this).data("mirror",mirror);
        });
    }

    
    self.initLog = function(){
        $(".log").each(function(){
            var mirror = CodeMirror.fromTextArea($("textarea",this)[0], {
                mode: "text/plain",
                lineNumbers: true,
                lineWrapping: true,
                readOnly: true
            });
            $(this).data("mirror",mirror);
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
