HTMLWidgets.widget({

  name: "LACE",

  type: "output",

  factory: function (el, width, height) {


    return {

      renderValue: function (jsdata) {
        var D = jsdata["data"]
        var info = jsdata["info"]
        var E = jsdata["columns"]
        var F = jsdata["colors"]

        // set the dimensions and margins of the graph
        var margin = {
            top: 20,
            right: 30,
            bottom: 20,
            left: 10
          },
          width = 460 - margin.left - margin.right,
          height = 800 - margin.top - margin.bottom;

        // ################
        // global variable vizObj
        vizObj = {};

        var xxx = {};

        // note current width & height in vizObj
        vizObj.cur_height = height / 3;
        vizObj.cur_width = width;
        //userConfig={};
        var userConfig = jsdata["x"]

        // ################


        //List of groups = header of the csv files
        var keys = E.slice(1).reverse()



        // Three function that change the tooltip when user:
        // hover / move / leave a cell
        var mouseover = function (d) {}
        var tip = {};
        var makeTippy = function (node, text) {

          var ref = node.popperRef();

          // unfortunately, a dummy element must be passed
          // as tippy only accepts a dom element as the target
          // https://github.com/atomiks/tippyjs/issues/661
          var dummyDomEle = document.createElement('div');

          var tip = tippy(dummyDomEle, {
            onCreate: function (instance) { // mandatory
              // patch the tippy's popper reference so positioning works
              // https://atomiks.github.io/tippyjs/misc/#custom-position
              instance.popperInstance.reference = ref;
            },
            lazy: false, // mandatory
            trigger: 'manual', // mandatory

            // dom element inside the tippy:
            content: function () { // function can be better for performance
              var div = document.createElement('div');

              div.innerHTML = text;

              return div;
            },

            // your own preferences:
            arrow: true,
            placement: 'bottom',
            hideOnClick: false,
            multiple: true,
            sticky: true,

            // if interactive:
            interactive: true,
            appendTo: document.body // or append dummyDomEle to document.body
          });

          return tip;
        };

        var mousemove = function (d, i) {
          grp = keys[i]
          cy.edges().style('opacity', '0.2');
          cy.nodes().forEach(function (ele) {
            if (ele.id().indexOf(keys[i]) !== -1) {
              ele.style('opacity', '1');
              if (tip[ele.id()] !== undefined) {
                tip[ele.id()].destroy();
              }


              tip[ele.id()] = makeTippy(ele, ele.data()["prev"]);
              tip[ele.id()].show();
              setTimeout(function () {
                tip[ele.id()].destroy();
              }, 4000);
            } else {
              ele.style('opacity', '0.2');
            }
          });
          cy.$(':parent').style('opacity', '1');
        }
        var mouseleave = function (d) {
          cy.nodes().style('opacity', '1');
          cy.edges().style('opacity', '1');
        }

        var tickmouseover = function (d) {
          cy.nodes().style('opacity', '0.2');
          cy.edges().style('opacity', '0.2');
          cy.$('#T' + d).style('opacity', '1');
          cy.$('#T' + d).children().style('opacity', '1');
          cy.$('#T' + d).children().connectedEdges().style('opacity', '1');

          cy.$('#T' + d).children().forEach(function (ele) {
            if (tip[ele.id()] !== undefined) {
              tip[ele.id()].destroy();
            }


            tip[ele.id()] = makeTippy(ele, ele.data()["prev"]);
            tip[ele.id()].show();
            setTimeout(function () {
              tip[ele.id()].destroy();
            }, 4000);
          });




        }

        var tickmouseout = function (d) {

          cy.nodes().style('opacity', '1');
          cy.edges().style('opacity', '1');
        }



        var mutdict = {}
        var elid = el.id


        var cy = window.cy = cytoscape({
          container: document.getElementById('cy'),
          boxSelectionEnabled: false,
          autounselectify: false,
          layout: {
            name: 'dagre',
            nodeSep: '100'
          },
          style: [
            {
              selector: 'node',
              css: {
                'content': 'data(name)',
                'text-valign': 'center',
                'width': 'data(size)',
                'background-color': 'data(color)',
                'height': 'data(size)',
                'text-halign': 'center',
                'font-size': '20',
              }
            },
            {
              selector: ':parent',
              css: {
                'content': 'data(name)',
                'text-valign': 'top',
                'background-opacity': '0.333',
                'text-halign': 'center',
                'border-width': '2',
                'border-color': 'black'
              }
            },
            {
              selector: '.fade',
              style: {
                'opacity': '0.5',
              }
             },
            {
              selector: 'edge',
              css: {
                'content': 'data(name)',
                'text-valign': 'center',
                'curve-style': 'bezier',
                'line-color': 'data(color)',
                'target-arrow-shape': 'triangle',
                'target-arrow-color': 'data(color)',
                'line-style': 'data(linestyle)',
                'font-size': '20',
              }
            }
          ],

          elements: jsdata["elements"],


          ready: function () {
            cy = this;

            function runLayout(fit, callBack) {
              var parentNodes = cy.nodes(':parent');
              var dagre_layout = parentNodes.layout({
                name: 'dagre',
                rankDir: 'LR',
                fit: fit
              });
              dagre_layout.promiseOn('layoutstop').then(function (event) {
                if (callBack) {
                  callBack.call(cy, event);
                }
              });
              dagre_layout.run();

            }

            function addlegend() {
              var tablecontainer = document.getElementById("lacetable")
              tablecontainer.style.visibility = 'visible'
              //tablecontainer.style["overflow-y"]="scroll"
              tablecontainer.style["overflow-y"] = null
              tablecontainer.style.left = "12px";
              tablecontainer.style.right = "12px";
              tablecontainer.style.margin = "12px";

              tablecontainer = document.getElementById("table")
              tablecontainer.style.display = 'flex'
              tablecontainer = document.getElementById("legend")
              tablecontainer.style.display = 'flex'
              tablecontainer = document.getElementById("mutations")
              tablecontainer.style.display = 'flex'


              var ddy = 25
              var width = 340
              var legend = d3.select("#legend")
                .append("svg")
                .attr("width", width ) //+ margin.left + margin.right)
                .attr("height", 100+ margin.top) // + margin.bottom)
                .append("g")
                .attr("class", "legend")

              legend.append("text")
                .attr("x", width - 44).attr("y", 9).attr("dy", ".35em")
                .style("text-anchor", "end")
                .text('parental relations\n(somatic mutations)');

              legend.append("line") //making a line for legend
                .attr("x1", width - 28).attr("x2", width)
                .attr("y1", 10)
                .attr("y2", 10)
                .style("stroke", "5")
                .style("stroke", '#000000');


              legend.append("text")
                .attr("x", width - 44).attr("y", 9 + ddy).attr("dy", ".35em")
                .style("text-anchor", "end")
                .text('persistance relation');

              legend.append("line") //making a line for legend
                .attr("x1", width - 28)
                .attr("x2", width)
                .attr("y1", 10 + ddy)
                .attr("y2", 10 + ddy)
                .style("stroke-dasharray", "5,5")
                .style("stroke", '#000000');


              legend.append("text")
                .attr("x", width - 44)
                .attr("y", 9 + ddy * 2)
                .attr("dy", ".35em")
                .style("text-anchor", "end")
                .text('node size proportional to clone prevalence');

              legend.append("circle") //making a line for legend
                .attr("cx", width - 28 + 28 / 2)
                .attr("cy", 10 + ddy * 2)
                .attr("r", 8)
                .style("stroke", "5")
                .style("stroke", '#000000')
                .style("fill", 'none');

            }
            addlegend();

            function addtable() {
              var tablecontainer = document.getElementById("lacetable")
              tablecontainer.style.visibility = 'visible'
              //tablecontainer.style["overflow-y"]="scroll"
              tablecontainer.style["overflow-y"] = null
              tablecontainer.style.left = "12px";
              tablecontainer.style.right = "12px";
              tablecontainer.style.margin = "12px";

              tablecontainer = document.getElementById("table")
              tablecontainer.style.display = 'flex'
              tablecontainer = document.getElementById("legend")
              tablecontainer.style.display = 'flex'
              tablecontainer = document.getElementById("mutations")
              tablecontainer.style.display = 'flex'

              tablecontainer = document.getElementById("table")
              tablecontainer.innerHTML = '<table class="in_table"></table>'
            }
            addtable();
            var datatable = new DataTable('table', {
              perPageSelect: [10, 20, 30],
              searchable: false,
              footer: false,
              layout: {
                top: "",
                bottom: ""
              },
              data: jsdata["tab"],
            });

            var mutdict = {}
            var request = {}
            var request2 = {}

            function getGene() {
              if (jsdata["clone_labels"].length > 0) {
                for (const name of jsdata["clone_labels"]) {
                  request[name] = new XMLHttpRequest();
                  request[name].onreadystatechange = function () {
                    if (this.readyState == 4 && this.status == 200) {
                      console.log('1 richiesta fatta')

                      if (this.status === 200) {
                        var data = JSON.parse(this.responseText);
                        console.log('1 risposta');
                        console.log(data);
                        if (data[0] !== undefined) {
                          request2[name] = new XMLHttpRequest();
                          request2[name].onreadystatechange = function () {
                            if (this.readyState == 4 && this.status == 200) {
                              console.log('2 richiesta fatta')
                              if (this.status === 200) {
                                var data2 = JSON.parse(this.responseText);
                                console.log('data2');
                                console.log(data2);
                                mutdict[name] = data2;
                                console.log('mutdict2')
                                console.log(mutdict)
                                cy["mutdict"] = mutdict
                              }
                            }
                          }
                          request2[name].onerror = function (e) {
                            //alert("Error fetching " + url);
                          };
                          console.log('before data2');
                          console.log(data[0]);
                          request2[name].open('GET', 
                                              'http://rest.ensembl.org/lookup/id/' + 
                                              data[0]["id"] + 
                                              '?content-type=application/json', 
                                              true);
                          if (navigator.onLine) {
                            //console.log('online');
                            request2[name].send(null);
                          } else {
                            console.log('offline');
                          }

                        }
                      }
                    }
                  };
                  request[name].onerror = function (e) {
                    //alert("Error fetching " + url);
                  };
                  request[name].open('GET', 
                                     'https://rest.ensembl.org/xrefs/symbol/homo_sapiens/' + 
                                     name + 
                                     '?content-type=application/json', 
                                     true);
                  if (navigator.onLine) {
                    //console.log('online');
                    request[name].send(null);
                  } else {
                    console.log('offline');
                  }

                }

              }
            }

            cy['getGene'] = getGene

            if (Object.keys(mutdict).length === 0) {
              cy.getGene();
            }

            cy["keys"] = keys
            cy["colors"] = F

            info.help = "<h4>Help</h4><ul> \
                                <li>\
																clicking on edges, nodes and fishplot \
																areas shows mutation info from ensembl.com\
																</li>\
                                <li>\
																moving the cursor over edges shows \
																accumulated mutations\
																</li>\
                                <li>\
																moving the cursor over nodes and both \
																fishplot areas and timlines shows clonal \
																prevalences \
																</li>\
                                <li>\
																mutation gene names must be in gene symbol or \
																id ensebl id format.\
																</li>\
                              </ul>"
            info.info = "<h4>Dataset info</h4>" + info.info


            function addnavbar() {
              var navbar = document.getElementById("navbar");
              navbar.innerHTML = '<nav class="navbar navbar-dark bg-dark" \
                                  style="display: flex;justify-content: \
                                  flex-end; position:relative;">' +
                                  '<h2 style="color:#f5f6fa; position: \
                                  absolute;left: 12px;">LACE 2.0</h2>' +
                                  //'<button class="btn btn-outline-success" \
                                  //type="button" style="align-content: \
                                  //space-around; margin:5px" 
                                  //onclick="_sweepClick('+'xxx '+')">view\
                                  //</button>'+
                                  //'<button class="btn btn-outline-success" \
                                  //type="button" style="align-content: \
                                  //space-around; margin:5px" 
                                  //onclick="_downloadPNG(' + '\'container\' '\
                                  //+ ',' + '\'timescape_streamgraph.png\' ' \
                                  //+ ')">PNG</button>' +
                                  //'<button class="btn btn-outline-success" \
                                  //type="button" style="align-content:\ 
                                  //space-around; margin:5px" onclick=\
                                  //"downloadSVG('+'\'timescape_streamgraph\' '\
                                  //+')">SVG</button>'+
                                  '<button class="btn btn-outline-success" \
                                  type="button"  data-toggle="modal" \
                                  data-target=".bd-example-modal-lg" \
                                  style=\
                                  "align-content:space-around; margin:5px">\
                                  info</button>' +
                                  '<div class="modal fade bd-example-modal-lg" \
                                  tabindex="-1" role="dialog" \
                                  aria-labelledby="myLargeModalLabel" \
                                  aria-hidden="true"> \
                                  <div class="modal-dialog modal-lg" > \
                                  <div class="modal-content" \
                                  style="padding:10px;">' + info.info+'</div> \
                                  </div> \
                                  </div>' +
                                  '<button class="btn btn-outline-success" \
                                  type="button"  data-toggle="modal" \
                                  data-target=".bd-example-modal-help-lg" \
                                  style="align-content:space-around; \
                                  margin:5px">?</button>\
                                  <div class="modal fade bd-example-modal-help-lg" \
                                  tabindex="-1" role="dialog" \
                                  aria-labelledby="myLargeModalLabel" \
                                  aria-hidden="true"> \
                                  <div class="modal-dialog modal-lg" > \
                                  <div class="modal-content" \
                                  style="padding:10px;">' + info.help+'</div> \
                                  </div> \
                                  </div>' +
                                  '</nav>'
            }
            addnavbar();



            //edge mouse interaction
            cy.edges().on('mouseover', function (e) {
              var str = "";
              edge = cy.$('#' + e.target.id());
              edge_target_name = edge.target().data()["name"];

              clonal_nodes = cy.nodes('[name ="' + edge_target_name + '"]');
              clonal_path = clonal_nodes.predecessors().edgesTo(clonal_nodes);

              clone = clonal_nodes.union(clonal_path).union(cy.$(':parent'))
              most = cy.nodes().union(cy.edges()).difference(clone)
              most.style('opacity', '0.2');
              clone.style('opacity', '1');

              //if(edge.data()["name"] in mutdict) {
              edge.target().predecessors().forEach(function (ele) {
                //ele.style('opacity','1');
                if (ele.isEdge()) {
                  if (ele.data()["name"] !== "") {
                    str = '<button class="btn btn-outline-success" \
                    type="button" style="float:right;margin: 5px;font-size: 14px">' + 
                      ele.data()["name"] + "</button> " + str;
                  }
                }
              });
              //}
              console.log(str);
              //var tablecontainer =  document.getElementById("lacetable")
              var tablecontainer = document.getElementById("lacetable")
              tablecontainer.style.visibility = 'visible'
              tablecontainer.style["overflow-y"] = null


              tablecontainer = document.getElementById("table")
              tablecontainer.style.display = 'flex'
              tablecontainer = document.getElementById("legend")
              tablecontainer.style.display = 'flex'
              tablecontainer = document.getElementById("mutations")
              tablecontainer.style.display = 'flex'


              tablecontainer = document.getElementById("mutations")
              tablecontainer.innerHTML = str;


              _tsMouseoverGenotype(edge_target_name, xxx.obj.view_id);
              _showLabels(edge_target_name, xxx.obj.view_id);

            });
            cy.edges().on('mouseout', function (e) {
              cy.nodes().style('opacity', '1');
              cy.edges().style('opacity', '1');
              _tsMouseoutGenotype(xxx.obj.view_id);
              _hideLabels(xxx.obj.view_id);

            });

            runLayout(true);

            elem = document.getElementById("lacetable")
            elem.addEventListener('mouseleave', function (d) {
              tablecontainer = document.getElementById("table")
              tablecontainer.style.display = 'flex'
              tablecontainer = document.getElementById("legend")
              tablecontainer.style.display = 'flex'
              tablecontainer = document.getElementById("mutations")
              tablecontainer.style.display = 'flex'
            });



            //edge mouse interaction
            cy.edges().on('click', function (e) {
              console.log(mutdict)

              if (Object.keys(mutdict).length == 0) {
                console.log('relaunch getGene()')
                cy.getGene();
              } else {
                console.log('relaunch not needed')
              }

              if (cy.$('#' + e.target.id()).data()["name"] !== "") {
                if (cy.$('#' + e.target.id()).data()["name"] in mutdict) {
                  var tablecontainer = document.getElementById("lacetable")
                  tablecontainer.style.visibility = 'visible'
                  tablecontainer.style["overflow-y"] = null

                  tablecontainer = document.getElementById("table")
                  tablecontainer.style.display = 'flex'
                  tablecontainer = document.getElementById("legend")
                  tablecontainer.style.display = 'flex'
                  tablecontainer = document.getElementById("mutations")
                  tablecontainer.style.display = 'flex'

                  tablecontainer = document.getElementById("mutations")
                  tablecontainer.innerHTML = '<div class="card" style="width: 18rem;"><div class="card-body"><h5 class="card-title">' + mutdict[cy.$('#' + e.target.id()).data()["name"]]["display_name"] + '</h5><h6 class="card-subtitle mb-2 text-muted">' + mutdict[cy.$('#' + e.target.id()).data()["name"]]["id"] + '</h6><p class="card-text" style="font-size: 14px">' + mutdict[cy.$('#' + e.target.id()).data()["name"]]["description"] + '</p><a  style="font-size: 14px" href="https://asia.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=' + mutdict[cy.$('#' + e.target.id()).data()["name"]]["id"] + '" class="btn btn-outline-success" type="button">' + mutdict[cy.$('#' + e.target.id()).data()["name"]]["assembly_name"] + '</a><a  style="font-size: 14px" href="https://asia.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=' + mutdict[cy.$('#' + e.target.id()).data()["name"]]["id"] + '" class="btn btn-outline-success" type="button">' + mutdict[cy.$('#' + e.target.id()).data()["name"]]["source"] + '</a>  </div></div>'

                }
              }
            });

            //node mouse interaction
            var tip;
            cy.nodes().on('mouseover', function (e) {
              if (!cy.$("#" + e.target.id()).isParent()) {
                if (tip !== undefined) {
                  tip.destroy();
                }
                tip = makeTippy(cy.$("#" + e.target.id()), 
                                cy.$("#" + e.target.id()).data()["prev"]);
                tip.show();
              }
            });

            cy.nodes().on('mouseout', function (e) {
              if (tip !== undefined) {
                tip.destroy();
              }
            });


          }
        });
        document.getElementById(el.id).cy = cy;

        xxx.obj = _run_timescape(el.id, 
                                 vizObj.cur_width, 
                                 vizObj.cur_height, 
                                 userConfig);
      },

      resize: function (width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
