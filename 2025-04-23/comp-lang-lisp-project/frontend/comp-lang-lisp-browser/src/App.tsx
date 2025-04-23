import React, { useState, useEffect } from 'react';
import { StarIcon, ChatBubbleLeftIcon } from '@heroicons/react/24/outline';
import { StarIcon as StarIconSolid } from '@heroicons/react/24/solid';
import { Sun, Moon, Laptop, Search } from 'lucide-react';

interface Thread {
  id: string;
  title: string;
  posts: Post[];
  post_count: number;
  first_post_date: string;
  last_post_date: string;
}

interface Post {
  id: string;
  subject: string;
  author_id: string;
  date: string;
  display_date: string;
  content: string;
  references: string[];
  thread_id: string;
}

interface Author {
  id: string;
  name: string;
  email: string;
  post_count: number;
}

const ThreadItem: React.FC<{
  thread: Thread;
  isFavorite: boolean;
  onToggleFavorite: (threadId: string) => void;
}> = ({ thread, isFavorite, onToggleFavorite }) => {
  return (
    <div className="bg-white dark:bg-gray-800 shadow overflow-hidden sm:rounded-md p-4 hover:bg-gray-50 dark:hover:bg-gray-700 transition duration-150">
      <div className="flex justify-between items-start">
        <div className="flex-1">
          <h2 className="text-lg font-medium text-gray-900 dark:text-white">{thread.title}</h2>
          <div className="mt-2 flex items-center text-sm text-gray-500 dark:text-gray-400">
            <span>@eriknaggum</span>
            <span className="mx-2">•</span>
            <span>{new Date(thread.last_post_date).toLocaleDateString()}</span>
            <span className="mx-2">•</span>
            <div className="flex items-center">
              <ChatBubbleLeftIcon className="h-4 w-4 mr-1" />
              <span>{thread.post_count}</span>
            </div>
          </div>
          {thread.posts.length > 0 && (
            <p className="mt-2 text-sm text-gray-600 dark:text-gray-300 line-clamp-2">
              {thread.posts[0].content.split('\n').slice(5).join(' ').substring(0, 200)}...
            </p>
          )}
        </div>
        <button 
          onClick={() => onToggleFavorite(thread.id)}
          className="ml-4 text-gray-400 hover:text-yellow-500 focus:outline-none"
        >
          {isFavorite ? (
            <StarIconSolid className="h-6 w-6 text-yellow-500" />
          ) : (
            <StarIcon className="h-6 w-6" />
          )}
        </button>
      </div>
    </div>
  );
};

const App: React.FC = () => {
  const [threads, setThreads] = useState<Thread[]>([]);
  const [loading, setLoading] = useState<boolean>(true);
  const [searchQuery, setSearchQuery] = useState<string>('');
  const [timeFilter, setTimeFilter] = useState<string>('All Time');
  const [favorites, setFavorites] = useState<Set<string>>(new Set());
  const [theme, setTheme] = useState<string>('light');

  useEffect(() => {
    const fetchData = async () => {
      try {
        const response = await fetch('/data/processed_data.json');
        if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        const data = await response.json();
        setThreads(data.threads || []);
        setLoading(false);
      } catch (error) {
        console.error('Error fetching threads:', error);
        setLoading(false);
      }
    };

    fetchData();
  }, []);

  const filteredThreads = threads.filter(thread => {
    if (!searchQuery) return true;
    
    // Search in thread title
    if (thread.title.toLowerCase().includes(searchQuery.toLowerCase())) {
      return true;
    }
    
    // Search in post content
    for (const post of thread.posts) {
      if (post.content.toLowerCase().includes(searchQuery.toLowerCase())) {
        return true;
      }
    }
    
    return false;
  });

  const toggleFavorite = (threadId: string) => {
    setFavorites(prevFavorites => {
      const newFavorites = new Set(prevFavorites);
      if (newFavorites.has(threadId)) {
        newFavorites.delete(threadId);
      } else {
        newFavorites.add(threadId);
      }
      return newFavorites;
    });
  };

  const changeTheme = (newTheme: string) => {
    setTheme(newTheme);
    document.documentElement.classList.remove('light', 'dark');
    
    if (newTheme === 'system') {
      const systemTheme = window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
      document.documentElement.classList.add(systemTheme);
    } else {
      document.documentElement.classList.add(newTheme);
    }
  };

  return (
    <div className="min-h-screen bg-gray-50 dark:bg-gray-900">
      <header className="bg-white dark:bg-gray-800 shadow">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4 flex justify-between items-center">
          <h1 className="text-xl font-bold text-gray-900 dark:text-white">
            <span className="text-blue-600 dark:text-blue-400">(comp.lang.lisp)</span>
          </h1>
          
          <div className="flex items-center space-x-2">
            <div className="relative rounded-md shadow-sm">
              <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                <Search className="h-5 w-5 text-gray-400" />
              </div>
              <input
                type="text"
                className="focus:ring-blue-500 focus:border-blue-500 block w-full pl-10 pr-12 py-2 sm:text-sm border-gray-300 rounded-md dark:bg-gray-700 dark:border-gray-600 dark:text-white"
                placeholder="Search threads..."
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.target.value)}
              />
              <div className="absolute inset-y-0 right-0 flex items-center">
                <select
                  className="h-full py-0 pl-2 pr-7 border-transparent bg-transparent text-gray-500 dark:text-gray-300 sm:text-sm rounded-md"
                  value={timeFilter}
                  onChange={(e) => setTimeFilter(e.target.value)}
                >
                  <option>All Time</option>
                  <option>Last Week</option>
                  <option>Last Month</option>
                  <option>Last Year</option>
                </select>
              </div>
            </div>
            
            <button 
              className={`p-2 rounded-md ${theme === 'light' ? 'bg-blue-100 text-blue-600' : 'bg-gray-700 text-gray-300'}`}
              onClick={() => changeTheme('light')}
            >
              <Sun className="h-5 w-5" />
            </button>
            
            <button 
              className={`p-2 rounded-md ${theme === 'dark' ? 'bg-blue-100 text-blue-600' : 'bg-gray-700 text-gray-300'}`}
              onClick={() => changeTheme('dark')}
            >
              <Moon className="h-5 w-5" />
            </button>
            
            <button 
              className={`p-2 rounded-md ${theme === 'system' ? 'bg-blue-100 text-blue-600' : 'bg-gray-700 text-gray-300'}`}
              onClick={() => changeTheme('system')}
            >
              <Laptop className="h-5 w-5" />
            </button>
          </div>
        </div>
      </header>
      
      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
        {loading ? (
          <div className="flex justify-center items-center h-64">
            <div className="animate-spin rounded-full h-12 w-12 border-t-2 border-b-2 border-blue-500"></div>
          </div>
        ) : filteredThreads.length === 0 ? (
          <div className="text-center py-12">
            <p className="text-lg text-gray-600 dark:text-gray-300">No threads found matching your search criteria.</p>
          </div>
        ) : (
          <div className="space-y-4">
            {filteredThreads.map(thread => (
              <ThreadItem 
                key={thread.id} 
                thread={thread} 
                isFavorite={favorites.has(thread.id)}
                onToggleFavorite={toggleFavorite}
              />
            ))}
          </div>
        )}
      </main>
    </div>
  );
};

export default App;
