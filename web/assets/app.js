'use strict';

var thefuseproject;
(function (thefuseproject) {
    
    function AppViewModel($scope, $http, $timeout, apiRootUrl) {
        var self = this;
        self.rootEntry = new Entry(null, "", function() { return isDir(""); });
        self.rootEntry.refreshing(addMissingChilds, $timeout);
        
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
                url: apiRootUrl + '/is-dir/' + thefuseproject.normalizePath(path),
                method: 'GET',
                timeout: 200
            })
            .then(thefuseproject.mapData)
            .catch(function() { return false });
        }
        
        function listDir(path) {
            return $http({
                url: apiRootUrl + '/list-dir/' + thefuseproject.normalizePath(path),
                method: 'GET'
            })
            .then(thefuseproject.mapData);
        }
        
        function createListEntries(parent, filenames, autoRefresh) {
            if (!filenames) {
                filenames = [];
            }
            
            return filenames.map(function(filename) { 
                if (autoRefresh) {
                    var entry = new Entry(parent, filename, isDir);
                    entry.refreshing(addMissingChilds, $timeout);
                    return entry;
                }
                else {
                    return new Entry(parent, filename, isDir);
                }
            });
        }
        
        function addMissingChilds(parent) {
            if (!parent.expanded) {
                return;
            }
            
            listDir(parent.fullname).then(function(filenames) {
                createListEntries(parent, filenames, false).forEach(function(childToRefresh) {
                    var child = parent.entries.filter(function(c) { return c.fullname == childToRefresh.fullname});
                    if (child.length == 0) {
                        childToRefresh.refreshing(addMissingChilds, $timeout);
                        parent.entries.push(childToRefresh);
                    }
                });
            });
        }
    }
    
    function Entry(parent, filename, isDir) {
        var self = this;
        var refreshingFun = function() {};
        
        this.name = filename;
        this.fullname = getFullname();
        this.isDir = null;
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
    
    angular.module('thefuseproject', ['ui.bootstrap', 'ngAnimate'])
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

    thefuseproject.mapData = function (promise) {
        return promise.data;  
    };
    
    thefuseproject.normalizePath = function (path) {
        if (!path) {
            path = "#";
        }
        
        return encodeURIComponent(path);
    };

})(thefuseproject || (thefuseproject = {}));