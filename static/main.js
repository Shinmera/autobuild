$(function(){
    $("a.confirm").each(function(){
        var self = this;
        $(self).click(function(){
            var title = $(self).attr("title")[0].toLowerCase() + $(self).attr("title").slice(1);
            return confirm("Are you sure you want to "+title+"?");
        });
    });
})
