// Fetch all the issue data from a given user/repo pair, and saves it into
// db/user/repo/issues.json.  If the issue file already exists, fetch only the
// issues updated since the last sync, and merge into the file.

// Create the API client
var Github_api = require("github")
var github = new Github_api({
  protocol: "https",            // this might be the default, but hey

  // the docs mention you should set this, even though I use a token for auth so
  // they know who I am
  headers: {
    "user-agent": "fmdkdd/github-burndown-chart"
  },
})

// Authenticate using a token that's not in version control.
github.authenticate({
  type: "token",
  token: require("./auth.json")["token"],
})

// TODO: These should be command-line parameters
var user = "flycheck"
var repo = "flycheck"

// First, find out the time we synced this user/repo pair.  If the file does not
// exist, we must fetch all the issues anew.
var fs = require('fs')
var path = require('path')

var db_filename = path.join('db', user, repo, 'issues.json')

fs.readFile(db_filename, 'utf8', function(err, data) {
  if (err) {
    // No issues saved, then fetch all the issues
    if (err.code === 'ENOENT') {

      // Try to create the directories first, because if that fails, there's no
      // point fetching the issues.
      var mkdirp = require('mkdirp')
      mkdirp(path.dirname(db_filename), function(err) {
        if (err) { bailOrExit(err) }

        fetchIssuesSince(user, repo, null, function writeToFile(err, issues) {
          if (err) { bailOrExit(err) }

          // Sort the issues by reverse update time, always
          issues.sort(sortByReverseUpdateTime)

          // and write
          console.log(`Saving ${issues.length} issues to file ${db_filename}...`)
          fs.writeFile(db_filename, JSON.stringify(issues), 'utf8', bailOrExit)
        })
      })
    }

    // Other error, abort
    else {
      bailOrExit(err)
    }
  }

  // No error, then find the sync time and fetch only the newest issues
  else {
    console.log(`Found existing file ${db_filename}`)

    var old_issues = JSON.parse(data)

    // Sort in reverse chronological order, so the latest updated issues is the
    // first one.
    old_issues.sort(sortByReverseUpdateTime)
    var last_update = old_issues[0].updated_at

    fetchIssuesSince(user, repo, last_update, function(err, new_issues) {
      if (err) { throw err }

      // Merge old and new issues
      var merged_issues = mergeIssues(old_issues, new_issues)

      // and update the file
      console.log(`Saving ${merged_issues.length} issues to file ${db_filename}...`)
      fs.writeFile(db_filename, JSON.stringify(merged_issues), 'utf8', bailOrExit)
    })
  }
})

function sortByUpdateTime(a, b) {
  return new Date(a.updated_at) - new Date(b.updated_at)
}

function sortByReverseUpdateTime(a, b) {
  return -sortByUpdateTime(a, b)
}

function bailOrExit(err) {
  if (err) {
    throw err
    process.exit(1)
  }
  process.exit(0)
}

function fetchIssuesSince(user, repo, since, cb) {
  // Collect the extracted issues in this object
  var issues = []

  var options = {
    user: user,
    repo: repo,
    state: "all",
    per_page: 100,    // we want /all/ the issues here, so might as
                      // well get the max per API call
  }

  // If 'since' was specified, only fetch the issues from that date
  if (since) {
    options.since = since
    process.stdout.write(`Fetching Github issues for ${user}/${repo} since ${since}...`)
  }

  else {
    process.stdout.write(`Fetching all Github issues for ${user}/${repo}...`)
  }

  // Start the request for the first page for results
  github.issues.getForRepo(options, getIssues)

  function getIssues(err, res) {
    // Pass any error to the callback
    if (err) { cb(err) }

    // Write a dot for each page of results; it may take some time so this
    // provides feedback to the user
    process.stdout.write('.')

    // Discard most issue information.  We just want the dates, ids, and to know
    // if it's a pull request or not.
    res.forEach(issue => {
      issues.push({
        id: issue.id,
        number: issue.number,
        state: issue.state,
        pull_request: "pull_request" in issue,
        created_at: issue.created_at,
        closed_at: issue.closed_at,
        updated_at: issue.updated_at,
      })
    })

    // Fetch the next page if it exists
    if (github.hasNextPage(res)) {
      github.getNextPage(res, getIssues)
    }
    // Otherwise we are done, signal the callback
    else {
      process.stdout.write(`fetched ${issues.length} issues\n`)
      cb(undefined, issues)
    }
  }
}

// Merge fresh issues in the existing array of existing issues, while preserving
// the sorting order.
function mergeIssues(old, fresh) {

  // We want to take fresh issues from oldest to newest, as we will unshift them
  // in the old array, thus preserving the reverse chronological order.
  fresh.sort(sortByUpdateTime)

  // All issues have an id, and if any issue from fresh already exists in old,
  // then delete it in old.
  fresh.forEach(function(issue) {
    var i = -1

    // Use `some` to short-circuit the search on the `id` key.  It's like
    // indexOf, but keyed.
    old.some(function(old_issue, index) {
      i = index
      return old_issue.id === issue.id
    })

    // If we found a match
    if (i > -1) {
      // Remove this issue because we have the fresh one
      old.splice(i, 1)
    }

    // In all cases, add the fresh one at the front
    old.unshift(issue)
  })

  return old
}
