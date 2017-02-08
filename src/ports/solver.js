//Code courtesy of http://dancing-links.herokuapp.com
(function(){"use strict";function s(s,o){e[s]=o,n[s]=o;var u=r[s],a=i[s];delete r[s],delete i[s];if(u!=null)for(var f=0;f<u.length;++f)t.extendModule(u[f],s,a[f])}function u(e){if(typeof e!="string")throw new Error;var t=e,n=e.match(o);return n&&(t=n[1]),t}var e=Function("return this;")(),t={};t.interpolate=function(t){if(t==null)return null;for(var n=1,r=arguments.length;n<r;++n)t=t.replace("{"+(n-1)+"}",(arguments[n]||"").toString());return t},t.error=function(t){throw new Error(t.interpolate(Array.prototype.slice.call(arguments,1)))};var n={},r={},i={},o=/([^\\\/]+)(?:\.js)$/;e.require==undefined&&(e.require=function(e){return n[u(e)]}),t.extendModule=function(e,t,n){if(!n)throw new Error;var s=require(t);if(s==null){var o=u(t),a=r[o],f=i[o];a==null&&(r[o]=a=[],i[o]=f=[]),a.push(e),f.push(n||{})}else{e.prototype=Object.create(s.prototype);for(var l in n)e.prototype[l]=n[l]}},t.publisher=function(t){return t==undefined||typeof window!="undefined"&&window==window.window?s:function(e,n){t.exports=n}},t.publisher(typeof module!="undefined"?module:undefined)("Utils",t)})(),function(){"use strict";function t(e,t){this.val=e,this.cellNo=t}function n(){var e=require("./Network.js");this.network=new e,this.rowConstraints=[],this.colConstraints=[],this.regionConstraints=[],this.cellConstraints=[],this.choices=[],this.possibleEntries=["1","2","3","4","5","6","7","8","9"],this.setUpConstraints(9,9,9,this.possibleEntries),this.setUpChoices(81,this.possibleEntries)}function r(e){return e=e.replace(i,""),e.length>81&&(e=e.replace(/ /g,"")),e=e.replace(s,"."),e}var e=require("./Utils.js");t.prototype.execute=function(e){e[this.cellNo]=this.val},t.prototype.getRowNo=function(){return Math.floor(this.cellNo/9)},t.prototype.getColNo=function(){return this.cellNo%9},t.prototype.getRegionNo=function(){var e=Math.floor(this.cellNo%9/3),t=Math.floor(this.cellNo/27);return e+t*3},t.prototype.toString=function(){return this.val+" in cell "+this.cellNo},n.prototype.setUpChoices=function(e,n){for(var r=0;r<e;++r){this.choices[r]=[];for(var i=0;i<n.length;++i){var s=new t(n[i],r);this.choices[r][i]=this.network.add(s,this.rowConstraints[s.getRowNo()][i],this.colConstraints[s.getColNo()][i],this.regionConstraints[s.getRegionNo()][i],this.cellConstraints[s.cellNo])}}},n.prototype.setUpConstraints=function(e,t,n,r){var e=9,t=9,i=e*t,n=9,r=[1,2,3,4,5,6,7,8,9];for(var s=0;s<e;++s){this.rowConstraints[s]=[];for(var o=0;o<r.length;++o)this.rowConstraints[s][o]=this.network.ensureConstraint(r[o]+" in row "+s)}for(var s=0;s<t;++s){this.colConstraints[s]=[];for(var o=0;o<r.length;++o)this.colConstraints[s][o]=this.network.ensureConstraint(r[o]+" in col "+s)}for(var s=0;s<n;++s){this.regionConstraints[s]=[];for(var o=0;o<r.length;++o)this.regionConstraints[s][o]=this.network.ensureConstraint(r[o]+" in region "+s)}for(var s=0;s<i;++s)this.cellConstraints[s]=this.network.ensureConstraint("entry in cell "+s)},n.prototype.fix=function(e,t,n){var r=this.possibleEntries.indexOf(e);this.choices[t][r].choose(n)};var i=/[^0-9\. ]/g,s=/[0 \.]/g;n.prototype.solve=function(e,t,n){var i=[];e=r(e);for(var s=0;s<e.length;++s){var o=e.charAt(s);o!="."&&this.fix(o,s,i)}var u=this.network.solve(t,n);for(var s=0;s<i.length;++s)i[s].restore();var a=[];a.info=u.info;for(var s=0;s<u.length;++s){var f=e.split(""),l=u[s];for(var s=0;s<l.length;++s)l[s].execute(f);var c=f.join("");a.push(c)}return a},n.prototype.showBoard=function(e){e=r(e);var t=e.split("");for(var n=0;n<9&&n*9<=t.length;++n){var i=n*9;i+3<t.length&&(t[i+3]=" | "+t[i+3]),i+6<t.length&&(t[i+6]=" | "+t[i+6]),n>0&&i<t.length&&(t[i]="\n"+t[i],n%3==0&&(t[i]="\n----+-----+----"+t[i]))}return t.join("")},e.publisher(typeof module!="undefined"?module:undefined)("SudokuSolver",n)}(),function(){"use strict";function t(e,t,n,r){return e>=0&&e<n&&t>=0&&t<r}function n(e,t){this.color=e,this.fills=t}function r(e,t,n){var r=require("./Network.js");this.network=new r,this.x=e,this.y=t,this.shapes=n,this.cellConstraints=[],this.shapeConstraints=[],this.setUpCellConstraints(),this.setUpShapeConstraints(),this.setUpChoices()}function i(e,t,n,r){this.idx=e,this.shape=t,this.x=n,this.y=r}var e=require("./Utils.js");n.prototype.overflows=function(e,n,r,i){for(var s=0;s<this.fills.length;++s){var o=e+this.fills[s].x,u=n+this.fills[s].y;if(!t(o,u,r,i))return!0}return!1},n.prototype.paint=function(e,t,n,r,i){for(var s=0;s<this.fills.length;++s){var o=this.fills[s];e.fillStyle=this.color,e.fillRect(t+o.x*r,n+o.y*i,r,i),e.strokeRect(t+o.x*r,n+o.y*i,r,i)}},r.prototype.setUpCellConstraints=function(){for(var e=0;e<this.x*this.y;++e)this.cellConstraints[e]=this.network.ensureConstraint("cell "+e+" must be filled")},r.prototype.setUpShapeConstraints=function(){for(var e=0;e<this.shapes.length;++e)this.shapeConstraints[e]=this.network.ensureConstraint("shape "+e+" must be placed")},i.prototype.toString=function(){return"Shape "+this.idx+" can be placed at ("+this.x+", "+this.y+")"},i.prototype.paint=function(e,t,n,r,i){this.shape.paint(e,this.x*r+t,this.y*i+n,r,i)},r.prototype.setUpChoices=function(){for(var e=0;e<this.shapes.length;++e){var t=this.shapes[e];for(var n=0;n<this.x*this.y;++n){var r=n%this.x,s=Math.floor(n/this.y);if(t.overflows(r,s,this.x,this.y)==0){var o=[new i(e,this.shapes[e],r,s),this.shapeConstraints[e]];for(var u=0;u<t.fills.length;++u){var a=t.fills[u],f=a.x+r+(a.y+s)*this.x;o.push(this.cellConstraints[f])}this.network.add.apply(this.network,o)}}}},r.prototype.solve=function(){var e=this.network.solve();return e},r.Shape=n,e.publisher(typeof module!="undefined"?module:undefined)("PolyominoSolver",r)}(),function(){"use strict";function t(e,t){this.rowHeader=e,this.columnHeader=t;var n=this,r={onNodeHidden:function(t){t==n.rowChain?n.rowHeader!=null&&n.rowHeader.actives--:t==n.colChain&&n.columnHeader!=null&&n.columnHeader.actives--},onNodeRestored:function(t){t==n.rowChain?n.rowHeader!=null&&n.rowHeader.actives++:t==n.colChain&&n.columnHeader!=null&&n.columnHeader.actives++},onNodeSpliced:function(e){this.onNodeRestored(e)}},i=require("./CircularList.js");this.rowChain=new i(this,r),this.colChain=new i(this,r)}var e=require("./Utils.js");t.prototype.forEachColumn=function(e){this.rowChain.forEach(e)},t.prototype.addToHeadersChains=function(){this.rowHeader!=null&&this.rowChain.spliceInto(this.rowHeader.rowChain.previous),this.columnHeader!=null&&this.colChain.spliceInto(this.columnHeader.colChain.previous)},t.prototype.forEachRow=function(e){this.colChain.forEach(e)},t.prototype.hideFromColumn=function(e){e.push(this.colChain),this.colChain.hide()},t.prototype.hideFromRow=function(e){e.push(this.rowChain),this.rowChain.hide()},t.prototype.toString=function(){return"{"+this.rowHeader+" x "+this.columnHeader+"}"},e.publisher(typeof module!="undefined"?module:undefined)("TableNode",t)}(),function(){"use strict";function t(e){for(var t=0;t<e.length;++t)e[t].restore()}function n(){var e=[];return e.info={ranOutOfTime:!1,foundMaxSolutions:!1,backtrackings:0,startTime:(new Date).getTime()},e}function r(e,n,i,s,o){if(i.info.ranOutOfTime&&o!=null&&o<(new Date).getTime())return i.info.ranOutOfTime=!0,i;var u=e.minConstraint();return u==null?i.push(s):u.actives==0?i.info.backtrackings++:u.forEachSatisfyingChoice(function(u){if(i.info.ranOutOfTime&&o!=null&&o<(new Date).getTime())return i.info.ranOutOfTime=!0,!1;var a=s.slice();u.description!=null&&a.push(u.description);var f=[];u.choose(f),r(e,n,i,a,o),t(f);if(n!=null&&i.length>=n)return i.info.foundMaxSolutions=!0,!1}),i}function i(){var e=require("./TableNode.js");e.call(this),this.rowChain.enumerable=!1,this.colChain.enumerable=!1,this.constraints={}}var e=require("./Utils.js");e.extendModule(i,"./TableNode.js",{makeOptional:function(t){var n=this.ensureConstraint(t);n.optional==0&&(n.optional=!0,this.add(null,t))},add:function(t,n){var r=require("./Choice.js"),i=require("./Constraint.js"),s=new r(t);s.colChain.spliceInto(this.colChain.previous);var o=require("./TableNode.js");for(var u=1;u<arguments.length;++u){var a=arguments[u];if(a==null)throw new Error("constraint may not be null");a instanceof i==0&&(a=this.ensureConstraint(a)),s.satisfies(a)}return s},ensureConstraint:function(t){var n=require("./Constraint.js"),r=this.constraints[t];return r==undefined&&(r=new n(t),this.constraints[t]=r,r.rowChain.spliceInto(this.rowChain.previous)),r},isSolved:function(){return this.rowChain.next==this.rowChain},minConstraint:function(){var e=null,t=null;return this.forEachColumn(function(n){if(t==null||t>n.actives){e=n,t=e.actives;if(t==0)return!1}}),e},solve:function(e,t){var i=r(this,e?e:null,n(),[],t?(new Date).getTime()+t:undefined);return i.info.endTime=(new Date).getTime(),i},solveOnce:function(e){var t=r(this,1,n(),[],e?(new Date).getTime()+e:undefined);return t.info.endTime=(new Date).getTime(),t[0]},toString:function(){var e="";return this.forEachRow(function(t){e+=t.toString(),t.forEachColumn(function(t){e+="	"+t.toString()}),e+="\n"}),e}}),e.publisher(typeof module!="undefined"?module:undefined)("Network",i)}(),function(){"use strict";function t(e){var t=require("./TableNode.js");t.call(this),this.actives=0,this.description=e,this.optional=!1,this.colChain.enumerable=!1}var e=require("./Utils.js");e.extendModule(t,"./TableNode.js",{satisfy:function(t){this.hideFromRow(t),this.forEachSatisfyingChoice(function(e){e.remove(t)})},forEachSatisfyingChoice:function(t){this.forEachRow(function(e){var n=e.rowHeader;return t(n,e)})},toString:function n(){return this.description+"("+this.actives+")!"}}),e.publisher(typeof module!="undefined"?module:undefined)("Constraint",t)}(),function(){"use strict";var e=require("./Utils.js"),t=0,n=function(n,r){this.hidden=!1,this.id=++t,this.next=this,this.previous=this,this.lifecycleListener=r,this.data=n,this.enumerable=n!==undefined?!0:!1},r={"undefined":"{0}: Bad argument: parameter '{1}' must not be undefined.",notFunc:"{0}: Bad argument: parameter '{1}' must be a function. Was '{2}' (type {3}).",hidden:"{0}: Illegal State: this function may not be called from a hidden node."};n.prototype={CLASSNAME:"CircularList",ERROR_MESSAGES:r,forEach:function(n){typeof n!="function"&&e.error(r.notFunc,"forEach","func",n,typeof n),this.hidden!==!1&&e.error(r.hidden,"forEach");var i=this;do{var s=i;i=s.next;var o=s.enumerable?n(s.data,s):!0;if(o===!1)break}while(i!=this)},toString:function(){return"{circ#"+this.id+"}"},toArray:function(){var t=[];return this.forEach(function(e){t.push(e)}),t},spliceInto:function(t){var n=this.previous;this.previous.next=t.next,this.previous=t,t.next.previous=this,t.next=this,this.lifecycleListener!=null&&this.lifecycleListener.onNodeSpliced(this,n)},push:function(i,s){i===undefined&&e.error(r["undefined"],"add","data");var o=new n(i,s);return o.spliceInto(this.previous),o},isEmpty:function(){if(this.next==this&&this.enumerable==0)return!0;var t=!0;return this.forEach(function(){return t=!1,!1}),t},hide:function(){this.hidden===!1&&(this.hidden=!0,this.next.previous=this.previous,this.previous.next=this.next,this.lifecycleListener!=null&&this.lifecycleListener.onNodeHidden(this))},restore:function(){this.hidden===!0&&(this.hidden=!1,this.next.previous=this,this.previous.next=this,this.lifecycleListener!=null&&this.lifecycleListener.onNodeRestored(this))}},e.publisher(typeof module!="undefined"?module:undefined)("CircularList",n)}(),function(){"use strict";function t(e){var t=require("./TableNode.js");t.call(this),this.actives=0,this.description=e,this.rowChain.enumerable=!1}var e=require("./Utils.js");e.extendModule(t,"./TableNode.js",{choose:function(t){this.forEachSatisfiedConstraint(function(e){e.satisfy(t)})},remove:function(t){this.hideFromColumn(t),this.forEachSatisfiedConstraint(function(e,n){n.hideFromColumn(t)})},satisfies:function(t){var n=require("./TableNode.js"),r=new n(this,t);return r.addToHeadersChains(),r},forEachSatisfiedConstraint:function(t){this.forEachColumn(function(e){var n=e.columnHeader;return t(n,e)})},toString:function n(){return this.description+"?"}}),e.publisher(typeof module!="undefined"?module:undefined)("Choice",t)}()

var sudokuSolver = new SudokuSolver()
var app = Elm.App.fullscreen()
app.ports.solve.subscribe(function(puzzle) {
  var solutions = sudokuSolver.solve(puzzle.join('').replace(/0/g,'.'), 20, 1000)
  app.ports.solutions.send(solutions.map(function(s) { return s.split('').map(Number) }))
})