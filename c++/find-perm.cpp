#include <iostream>
#include <future>
#include <thread>
#include <mutex>
#include <queue>

std::condition_variable cv, found_cv;
std::mutex queue_mutex, found_mutex;
std::queue<int> queue;
int ret;
bool found = false;

void search(int target) {
  while (!found) {

    int n=0;

    {
      std::unique_lock<std::mutex> lock(queue_mutex);
      cv.wait(lock, []{ return !queue.empty(); });
      n = queue.front();
      queue.pop();
    }

    if (n == target) {
      std::unique_lock<std::mutex> lock(found_mutex);
      found = true;
      ret = n;
      found_cv.notify_all();
    }
  }
}

int main() {

  int x = 232;

  int num_threads = std::thread::hardware_concurrency();

  printf("Using %d threads\n", num_threads);

  std::vector<std::thread> pool;

  for (int i=0; i < num_threads; ++i) {
    pool.push_back(std::thread(search, x));
  }

  for (int i=0; i < 500000; ++i) {
    {
      std::unique_lock<std::mutex> lock(queue_mutex);
      queue.push(i);
    }
    cv.notify_one();
  }

  {
    std::unique_lock<std::mutex> lock(found_mutex);
    found_cv.wait(lock, [] { return found; });
  }

  for (auto &t: pool) {
    t.join();
  }

  printf("Found %d\n", ret);
}
