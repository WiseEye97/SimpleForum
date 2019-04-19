
/*module.exports = {
    parseCode : function(){
        document.querySelectorAll('pre code').forEach((block) => {
            hljs.highlightBlock(block);
        });
    }
} */

export function parseCode(){
    var _ = window.setTimeout(() => {document.querySelectorAll('pre code').forEach((block) => {
        hljs.highlightBlock(block);
    });}, 100);
    
}