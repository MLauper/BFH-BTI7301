'use strict';

var lisptreeview;
(function (lisptreeview) {
    
    function AppViewModel($scope, $http, $timeout, apiRootUrl) {
        var self = this;
        self.rootEntry = new Entry(null, "", function() { return isDir(""); }, function() { return isSymLink(""); });
        self.rootEntry.refreshing(addRemoveChilds, $timeout);
        
        self.expand = function(entry) {
            entry.expanded = true;
            listDir(entry.fullname).then(function(res) {
                Array.prototype.push.apply(entry.entries, createListEntries(entry, res, true));
            });
        };
        
        self.collapse = function(entry) {
            entry.expanded = false;
            entry.entries.forEach(function(e) { 
                e.refreshing(false, $timeout);
                self.collapse(e);
            });
            entry.entries = [];
        };
        
        self.expand(self.rootEntry);

        function isDir(path) {
            return $http({
                url: apiRootUrl + '/is-dir/' + lisptreeview.normalizePath(path),
                method: 'GET'
            })
            .then(lisptreeview.mapData)
            .then(function(res) { return res === true; });
        }
        
        function isSymLink(path) {
            return $http({
                url: apiRootUrl + '/is-sylnk/' + lisptreeview.normalizePath(path),
                method: 'GET'
            })
            .then(lisptreeview.mapData)
            .then(function(res) { return res === true; });
        }
        
        function listDir(path) {
            return $http({
                url: apiRootUrl + '/list-dir/' + lisptreeview.normalizePath(path),
                method: 'GET'
            })
            .then(lisptreeview.mapData);
        }
        
        function createListEntries(parent, filenames, autoRefresh) {
            if (!filenames) {
                filenames = [];
            }
            
            return filenames.map(function(filename) { 
                if (autoRefresh) {
                    var entry = new Entry(parent, filename, isDir, isSymLink);
                    entry.refreshing(addRemoveChilds, $timeout);
                    return entry;
                }
                else {
                    return new Entry(parent, filename, isDir, isSymLink);
                }
            });
        }
        
        function addRemoveChilds(parent) {
            if (!parent.expanded) {
                return;
            }
            
            listDir(parent.fullname).then(function(filenames) {
                var childs = parent.entries;
                var newChilds = createListEntries(parent, filenames, false);
                
                childs.forEach(function(child) {
                    var newChild = newChilds.filter(function(c) { return c.fullname == child.fullname});
                    if (newChild.length == 0) {
                        self.collapse(child);
                        childs.splice(childs.indexOf(child), 1);
                    }
                });
                
                newChilds.forEach(function(childToRefresh) {
                    var child = childs.filter(function(c) { return c.fullname == childToRefresh.fullname});
                    if (child.length == 0) {
                        childToRefresh.refreshing(addRemoveChilds, $timeout);
                        childs.push(childToRefresh);
                    }
                });
            });
        }
    }
    
    function Entry(parent, filename, isDir, isSymLink) {
        var self = this;
        var refreshingFun = function() {};
        
        this.name = filename;
        this.fullname = getFullname();
        this.isDir = null;
        this.isSymLink = null;
        this.depth = parent ? parent.depth + 1 : 0;
        this.entries = [];
        this.expanded = false;
        
        this.refreshing = function(action, $timeout) {
            if (action) {
                $timeout.cancel(refreshingFun);
                refreshingFun = $timeout(function() { action(self); self.refreshing(action, $timeout); }, 5000);
            }
            else {
                $timeout.cancel(refreshingFun);
                refreshingFun = function() {};
            }
        };
            
        isDir(this.fullname).then(function(r) {
            self.isDir = r; 
        });
        
        isSymLink(this.fullname).then(function(r) {
            self.isSymLink = r; 
        });
        
        function getFullname() {
            if (parent == null || !parent.fullname) {
                return filename;
            }
            else {
                return parent.fullname + "/" + filename ;
            }
        }
    }
    
    function DebounceFactory($timeout) {
        return function(callback, interval) {
            var timeout = null;
            return function() {
                var args = arguments;
                $timeout.cancel(timeout);
                timeout = $timeout(
                    function () { callback.apply(this, args); }, 
                    interval
                );
            };
        }; 
    }
    
    angular.module('lisptreeview', ['ui.bootstrap', 'ngAnimate'])
        .constant("apiRootUrl", "http://benidev.collab.ch:8000/api")
        .controller('AppViewModel', AppViewModel)
        .factory('debounce', DebounceFactory)
        .run(function($rootScope, $rootElement, apiRootUrl) {
            $rootScope.$ignore = function() { return false };
            $rootScope.$today = moment().startOf('day').toDate();
            $rootScope.$tomorrow = moment().add(1, 'day').startOf('day').toDate();
            $rootScope.$yesterday = moment().add(-1, 'day').startOf('day').toDate();
            $rootScope.apiRootUrl = apiRootUrl;
        });

    lisptreeview.mapData = function (promise) {
        return promise.data;  
    };
    
    lisptreeview.normalizePath = function (path) {
        if (!path) {
            path = "#";
        }
        
        return encodeURIComponent(path);
    };

})(lisptreeview || (lisptreeview = {}));