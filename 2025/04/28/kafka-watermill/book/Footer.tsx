"use client";

import Navbar from "../components/Navbar";

export default function Footer() {
  return (
    <footer className="bg-gray-800 text-white py-8">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
          <div>
            <h3 className="text-lg font-semibold mb-4">About the Book</h3>
            <p className="text-gray-300">
              Polyglot Event-Driven Systems with Kafka: A comprehensive guide to building resilient, 
              scalable systems using Go, Kotlin, and Ruby with Apache Kafka.
            </p>
          </div>
          <div>
            <h3 className="text-lg font-semibold mb-4">Quick Links</h3>
            <ul className="space-y-2">
              <li><a href="/" className="text-gray-300 hover:text-white">Home</a></li>
              <li><a href="/about" className="text-gray-300 hover:text-white">About the Book</a></li>
              <li><a href="/sample-chapters" className="text-gray-300 hover:text-white">Sample Chapters</a></li>
              <li><a href="/resources" className="text-gray-300 hover:text-white">Resources</a></li>
            </ul>
          </div>
          <div>
            <h3 className="text-lg font-semibold mb-4">Connect</h3>
            <ul className="space-y-2">
              <li><a href="https://kafka.apache.org/" target="_blank" rel="noopener noreferrer" className="text-gray-300 hover:text-white">Apache Kafka</a></li>
              <li><a href="https://github.com/apache/kafka" target="_blank" rel="noopener noreferrer" className="text-gray-300 hover:text-white">Kafka on GitHub</a></li>
              <li><a href="https://www.confluent.io/blog/" target="_blank" rel="noopener noreferrer" className="text-gray-300 hover:text-white">Confluent Blog</a></li>
            </ul>
          </div>
        </div>
        <div className="mt-8 pt-8 border-t border-gray-700 text-center text-gray-400">
          <p>© {new Date().getFullYear()} Polyglot Kafka Book. All rights reserved.</p>
        </div>
      </div>
    </footer>
  );
}
