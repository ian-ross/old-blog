'use strict';

function EgCtrl(plotLib, $http, $scope, $location, $timeout) {
  plotLib.midMonths = function(ms, y) {
    return ms.map(function(m) { return new Date(y, m, 15); });
  };

  // Turn a vector of [[x1,y1], [x2,y2], ..., [xn,yn]] into a vector
  // of y-values interpolated to [f, ..., t].
  plotLib.fillIn = function(d, f, t) {
    var ft = t - f + 1;
    var ys = new Array(ft);
    var x1 = d[0][0], xn = d[d.length-1][0];
    var y1 = d[0][1], yn = d[d.length-1][1];
    if (d.length == 1)
      for (var i = 0; i < ft; ++i) ys[i] = y1;
    else {
      var i = 0;
      if (f < x1) {
        var delta = (d[1][1] - y1) / (d[1][0] - x1);
        var yf = y1 - delta * (x1 - f);
        for (; i < x1-f; ++i) ys[i] = yf + delta * i;
      }
      ys[i] = y1;
      var j = 1;
      while (j < d.length) {
        var ym = d[j-1][1], yp = d[j][1], xm = d[j-1][0], xp = d[j][0];
        var delta = (yp - ym) / (xp - xm);
        for (; x1+i < d[j][0]; ++i) ys[i] = ym + delta * (x1+i - xm);
        if (i < ft) ys[i++] = d[j++][1];
      }
      if (i < ft) {
        var delta = (yn - d[d.length-2][1]) / (xn - d[d.length-2][0]);
        for (var i0 = i; i < ft; ++i) ys[i] = yn + delta * (i-i0+1);
      }
    }
    return ys;
  };

  $scope.$watch('$location.hash', function() {
    var url = "http://" + location.host + "/radian/eg/" +
      location.hash.slice(2) + ".html";
    $timeout(function() {
      $http.get(url).success(function(res) {
        res = res.replace(/<h3>(.|\n)*<\/h3>\n\n/m, "");
        $('div#main-container pre.include-source').remove();
        var ctr = $('div#main-container');
        ctr.append('<pre class="include-source">' +
                   '<code class="html"></code></pre>');
        var code = $($(ctr.children()[ctr.children().length-1]).children()[0]);
        code.text(res);
        code.highlight();
      }, 0);
    });
  });
}
EgCtrl.$inject = ['plotLib', '$http', '$scope', '$location', '$timeout'];

var negs = 29;
var egtitles = [ "Basic plot; CSV data", // 1
                 "Basic plot; JSON data", // 2
                 "Interactive legend; stroke fading", // 3
                 "X-axis zoom", // 4
                 "Stroke fade UI", // 5
                 "Stroke colour selection UI", // 6
                 "Date handling", // 7
                 "Data aggregation", // 8
                 "Bar charts", // 9
                 "Functional plots #1", // 10
                 "Functional plots #2", // 11
                 "Expression vectorisation", // 12
                 "Data binding", // 13
                 "<plot-options> directive", // 14
                 "Another <plot-options> example", // 15
                 "Basic points plot", // 16
                 "Log axes", // 17
                 "Second axes", // 18
                 "Bar chart", // 19
                 "Basic palette", // 20
                 "Discrete palette", // 21
                 "Discrete palette (mark)", // 22
                 "Functional + palette", // 23
                 "Histogram", // 24
                 "Banded palettes", // 25
                 "Data access via URL", // 26
                 "Area plot", // 27
                 "More functional palettes", // 28
                 "Wealth of Nations" ]; // 29

angular.module('myApp', ['radian']).
  config(['$routeProvider', function($routeProvider) {
    for (var eg = 1; eg <= negs; ++eg) {
      var n = (eg < 10 ? '0' : '') + eg;
      $routeProvider.when('/' + n, { templateUrl: 'eg/' + n + '.html',
                                     controller: EgCtrl });
    }
    $routeProvider.otherwise({ redirectTo: '/01' });
  }]).
  controller('BaseController', ['$rootScope', function($rootScope) {
    $rootScope.egs = [];
    for (var eg = 1; eg <= negs; ++eg) {
      var n = (eg < 10 ? '0' : '') + eg;
      $rootScope.egs.push({ link: "#/" + n, title: egtitles[eg - 1] });
    }
  }]);


/* ========================================================
 * bootstrap-tab.js v2.1.1
 * http://twitter.github.com/bootstrap/javascript.html#tabs
 * ========================================================
 * Copyright 2012 Twitter, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * ======================================================== */


!function ($) {

  "use strict"; // jshint ;_;


  /* TAB CLASS DEFINITION
   * ==================== */

  var Tab = function (element) {
    this.element = $(element)
  }

  Tab.prototype = {

    constructor: Tab

    , show: function () {
      var $this = this.element
        , $ul = $this.closest('ul:not(.dropdown-menu)')
        , selector = $this.attr('data-target')
        , previous
        , $target
        , e

      if (!selector) {
        selector = $this.attr('href')
        selector = selector && selector.replace(/.*(?=#[^\s]*$)/, '') //strip for ie7
      }

      if ( $this.parent('li').hasClass('active') ) return

      previous = $ul.find('.active a').last()[0]

      e = $.Event('show', {
        relatedTarget: previous
      })

      $this.trigger(e)

      if (e.isDefaultPrevented()) return

      $target = $(selector)

      this.activate($this.parent('li'), $ul)
      this.activate($target, $target.parent(), function () {
        $this.trigger({
          type: 'shown'
          , relatedTarget: previous
        })
      })
    }

    , activate: function ( element, container, callback) {
      var $active = container.find('> .active')
        , transition = callback
                    && $.support.transition
                    && $active.hasClass('fade')

      function next() {
        $active
          .removeClass('active')
          .find('> .dropdown-menu > .active')
          .removeClass('active')

        element.addClass('active')

        if (transition) {
          element[0].offsetWidth // reflow for transition
          element.addClass('in')
        } else {
          element.removeClass('fade')
        }

        if ( element.parent('.dropdown-menu') ) {
          element.closest('li.dropdown').addClass('active')
        }

        callback && callback()
      }

      transition ?
                       $active.one($.support.transition.end, next) :
                       next()

      $active.removeClass('in')
    }
  }


  /* TAB PLUGIN DEFINITION
   * ===================== */

  $.fn.tab = function ( option ) {
    return this.each(function () {
      var $this = $(this)
        , data = $this.data('tab')
      if (!data) $this.data('tab', (data = new Tab(this)))
      if (typeof option == 'string') data[option]()
    })
  }

  $.fn.tab.Constructor = Tab


  /* TAB DATA-API
   * ============ */

  $(function () {
    $('body').on('click.tab.data-api', '[data-toggle="tab"], [data-toggle="pill"]', function (e) {
      e.preventDefault()
      $(this).tab('show')
    })
  })

}(window.jQuery);
