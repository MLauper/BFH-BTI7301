'use strict';

var thefuseproject;
(function (thefuseproject) {
    
    function AppViewModel($scope, $http, apiRootUrl) {
        var self = this;
        self.rootEntry = new Entry(null, "", function() { return isDir(""); });
        
        self.expand = function(entry) {
            entry.expanded = true;
            listDir(entry.fullname).then(function(res) {
                Array.prototype.push.apply(entry.entries, createListEntries(entry, res));
            });
        };
        
        self.collapse = function(entry) {
            entry.expanded = false;
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
        
        function createListEntries(parent, filenames) {
            if (!filenames) {
                return [];
            }
            else {
                return filenames.map(function(filename) { 
                    return new Entry(parent, filename, isDir);
                });
            }
        }
    }
    
    function Entry(parent, filename, isDir) {
        var self = this;
        
        this.name = filename;
        this.fullname = getFullname();
        this.isDir = null;
        this.depth = parent ? parent.depth + 1 : 0;
        this.entries = [];
        this.expanded = false;
            
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