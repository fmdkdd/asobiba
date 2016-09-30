document.addEventListener('DOMContentLoaded', load_data)

function load_data() {
  var file = document.querySelector('#file-input').files[0]

  var reader = new FileReader()
  reader.onload = function(e) { redraw_graphs(e.target.result) }
  reader.onerror = function(err) { console.error(err) }
  reader.readAsText(file)
}

function make_graph(data, keys, options) {
  options = options || {}
  options.margin = options.margin || {}
  options.margin.top = options.margin.top || 20
  options.margin.right = options.margin.right || 40
  options.margin.bottom = options.margin.bottom || 40
  options.margin.left = options.margin.left || 20

  // SVG dimensions
  options.width = options.width || 900
  options.height = options.height || 400

  var svg = d3.select("body").append("svg")
      .attr("width", options.width)
      .attr("height", options.height)
      .append("g")
      .attr("transform", `translate(${options.margin.left}, ${options.margin.top})`)

  // Nice colors provided by d3
  options.area_colors = options.area_colors || d3.schemeCategory20

  // Accept a single key instead of the array
  if (!Array.isArray(keys))
    keys = [keys]

  // Drawing area dimensions
  var width = options.width - options.margin.left - options.margin.right
  var height = options.height - options.margin.top - options.margin.bottom

  // Data is sorted by time, so the first point is always the first issue
  var earliest = data[0].time

  // X and Y axes
  var x = d3.scaleTime()
      .domain([earliest, Date.now()])
      .range([0, width])

  // Find the upper point of the graph in all the data points, for all the keys
  var maxKey = (a) => keys.reduce((m, k) => Math.max(m, k(a)), 0)
  var max = data.reduce((m, a) => Math.max(m, maxKey(a)), 0)

  var y = d3.scaleLinear()
      .domain([0, max])
      .range([height, 0])   // reversed because SVG origin is top-left

  // Draw the axes
  svg.append("g")
    .attr("class", "x axis")
    .attr("transform", `translate(0, ${height})`)
    .call(d3.axisBottom(x))

  svg.append("g")
    .attr("class", "y axis")
    .attr("transform", `translate(${width}, 0)`)
    .call(d3.axisRight(y))

  // Draw the areas
  keys.forEach((key, i) => {
    var area = d3.area()
        .x(function(d) { return x(d.time) })
        .y1(function(d) { return y(key(d)) })
        .y0(y(0))

    svg.datum(data)
      .append("path")
      .attr("d", area)
      .attr("fill", options.area_colors[i])
  })
}

function make_stacked_graph(data, keys, options) {
  options = options || {}
  options.margin = options.margin || {}
  options.margin.top = options.margin.top || 20
  options.margin.right = options.margin.right || 40
  options.margin.bottom = options.margin.bottom || 40
  options.margin.left = options.margin.left || 20

  // SVG dimensions
  options.width = options.width || 900
  options.height = options.height || 400

  var svg = d3.select("body").append("svg")
      .attr("width", options.width)
      .attr("height", options.height)
      .append("g")
      .attr("transform", `translate(${options.margin.left}, ${options.margin.top})`)

  // Nice colors provided by d3
  options.area_colors = options.area_colors || d3.schemeCategory20

  // Drawing area dimensions
  var width = options.width - options.margin.left - options.margin.right
  var height = options.height - options.margin.top - options.margin.bottom

  // Data is sorted by time, so the first point is always the first issue
  var earliest = data[0].time

  // X and Y axes
  var x = d3.scaleTime()
      .domain([earliest, Date.now()])
      .range([0, width])

  // Find the upper point of the graph in all the data points, for all the keys
  var maxKey = (a) => keys.reduce((m, k) => m + a[k], 0)
  var max = data.reduce((m, a) => Math.max(m, maxKey(a)), 0)

  var y = d3.scaleLinear()
      .domain([0, max])
      .range([height, 0])   // reversed because SVG origin is top-left

  // Draw the axes
  svg.append("g")
    .attr("class", "x axis")
    .attr("transform", `translate(0, ${height})`)
    .call(d3.axisBottom(x))

  svg.append("g")
    .attr("class", "y axis")
    .attr("transform", `translate(${width}, 0)`)
    .call(d3.axisRight(y))

  // Draw the areas
  var area = d3.area()
      .x(d => x(d.data.time))
      .y0(d => y(d[0]))
      .y1(d => y(d[1]))

  var stack = d3.stack().keys(keys)
  svg.selectAll("layer")
    .data(stack(data))
    .enter()
    .append("path")
    .attr("d", area)
    .attr("fill", d => options.area_colors[d.key])
}

function redraw_graphs(data) {
  d3.selectAll("svg").remove()
  var c = d3.schemeCategory20

  var points = prepare_data(JSON.parse(data))

  make_stacked_graph(points,
                     ["closed_prs", "open_prs", "closed_issues", "open_issues"],
                     {
                       height: 300,
                       area_colors: {
                         "closed_prs": c[9],
                         "open_prs": c[8],
                         "closed_issues": c[1],
                         "open_issues": c[0],
                       }
                     })

  make_stacked_graph(points,
                     ["open_prs", "open_issues"],
                     {
                       height: 200,
                       area_colors: {
                         "open_prs": c[8],
                         "open_issues": c[0],
                       }
                     })

  // make_graph(points,
  //            [p => p.open_issues + p.closed_issues, p => p.closed_issues],
  //            {height: 300, area_colors: [c[0], c[1]] })
}

function prepare_data(issues) {
  // First convert all issues to events.  The point is to organize all these
  // events along time, to be able to count the running total afterwards.
  var events = []

  issues.forEach(function issueToEvents(i) {
    // Emit an event for each new issue
    events.push({
      type: i.pull_request ? "new pull request" : "new issue",
      time: new Date(i.created_at),
    })

    // and for each closed issue
    if (i.closed_at) {
      events.push({
        type: i.pull_request ? "closed pull request" : "closed issue",
        time: new Date(i.closed_at),
      })
    }
  })

  // Now sort the events by time.  Implicitly, new issues are already sorted
  // chronologically, but closed issues are not.
  events.sort(function sortByTime(a, b) {
    return a.time - b.time
  })


  // Then count the running total of issues vs. running total of closed issues
  var points = []
  var open_issues = 0
  var open_prs = 0
  var closed_issues = 0
  var closed_prs = 0

  // Emit a point for each event.  Points will be chronologically sorted
  events.forEach(function eventsToPoints(e) {
    if (e.type === "new issue") {
      ++open_issues
      points.push({time: e.time, open_issues, closed_issues, open_prs, closed_prs})
    }

    else if (e.type == "new pull request") {
      ++open_prs
      points.push({time: e.time, open_issues, closed_issues, open_prs, closed_prs})
    }

    else if (e.type == "closed issue") {
      --open_issues
      ++closed_issues
      points.push({time: e.time, open_issues, closed_issues, open_prs, closed_prs})
    }

    else if (e.type === "closed pull request"){
      --open_prs
      ++closed_prs
      points.push({time: e.time, open_issues, closed_issues, open_prs, closed_prs})
    }
  })

  return points
}
