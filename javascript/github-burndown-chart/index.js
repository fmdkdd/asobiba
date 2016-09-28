// Fetch all the issue data from a given user/repo pair, and saves it into
// db/user/repo/issues.json.  If the issue file already exists, fetch only the
// issues updated since the last sync, and merge into the file.

// Parse args
var neodoc = require('neodoc')

var args = neodoc.run(`
usage: fetch-issues <user> <repository>
`)

var user = args['<user>']
var repo = args['<repository>']

var path = require('path')
var db_filename = path.join('db', user, repo, 'issues.json')

// First, find out the time we synced this user/repo pair.  If the file does not
// exist, we must fetch all the issues anew.

var P = require('bluebird')
var fs = P.promisifyAll(require('fs'))

fs.readFileAsync(db_filename, 'utf8')

  // If the file exists, then parse it, find the sync time and fetch only the
  // newest issues
  .then(JSON.parse)
  .then(old_issues => {
    console.log(`Found existing file ${db_filename}`)

    // Sort in reverse chronological order, so the latest updated issues is the
    // first one.
    old_issues.sort(sortByReverseUpdateTime)
    var last_update = old_issues[0].updated_at

    return P.join(old_issues, fetchIssuesSinceAsync(user, repo, last_update))
  })

  .then(([old_issues, new_issues]) => {
    // Merge old and new issues
    var issues = mergeIssues(old_issues, new_issues)

    // and update the file
    console.log(`Saving ${issues.length} issues to file ${db_filename}...`)
    return fs.writeFile(db_filename, JSON.stringify(issues), 'utf8')
  })

  .catch(SyntaxError, err => {
    console.error(`JSON parse error on file ${db_filename}.  Remove this file and retry to fetch a new one.`)
    process.exit(1)
  })

  // No issues saved, then fetch all the issues
  .catch({code: 'ENOENT'}, err => {

    // Create the directories first, because if that fails, there's no
    // point fetching the issues.
    var mkdirpAsync = P.promisify(require('mkdirp'))
    return mkdirpAsync(path.dirname(db_filename))
      .then(() => fetchIssuesSinceAsync(user, repo, null))
      .then(issues => {
        // Sort the issues by reverse update time, always
        issues.sort(sortByReverseUpdateTime)

        // and write
        console.log(`Saving ${issues.length} issues to file ${db_filename}...`)
        return fs.writeFileAsync(db_filename, JSON.stringify(issues), 'utf8')
      })
  })

  .catch({code: 404}, err => {
    console.error(`Unknown repository: ${user}/${repo}`)
    process.exit(1)
  })

function sortByUpdateTime(a, b) {
  return new Date(a.updated_at) - new Date(b.updated_at)
}

function sortByReverseUpdateTime(a, b) {
  return -sortByUpdateTime(a, b)
}

var fetchIssuesSinceAsync = P.promisify(fetchIssuesSince)

function fetchIssuesSince(user, repo, since, cb) {
  // Collect the extracted issues in this object
  var issues = []

  // Create the API client
  var Github_api = require("github")
  var github = new Github_api({
    protocol: "https",            // this might be the default, but hey

    // the docs mention you should set this, even though I use a token for auth so
    // they know who I am
    headers: {
      "user-agent": "fmdkdd/github-burndown-chart"
    },

    Promise: P,
  })

  // Authenticate using a token that's not in version control.
  github.authenticate({
    type: "token",
    token: require("./auth.json")["token"],
  })

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
  return github.issues.getForRepo(options).then(getIssues, cb)

  function getIssues(res) {
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
      return github.getNextPage(res).then(getIssues, cb)
    }
    // Otherwise we are done, signal the callback
    else {
      process.stdout.write(`fetched ${issues.length} issues\n`)
      return cb(undefined, issues)
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
