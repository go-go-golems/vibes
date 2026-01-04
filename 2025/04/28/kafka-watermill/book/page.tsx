import Navbar from "../components/Navbar";
import Footer from "../components/Footer";
import Link from "next/link";

export default function Home() {
  return (
    <div className="flex flex-col min-h-screen">
      <Navbar />
      <main className="flex-grow">
        {/* Hero Section */}
        <div className="bg-gradient-to-r from-red-600 to-red-800 text-white py-20">
          <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 text-center">
            <h1 className="text-4xl font-extrabold tracking-tight sm:text-5xl lg:text-6xl">
              Polyglot Event-Driven Systems with Kafka
            </h1>
            <p className="mt-6 text-xl text-red-100 max-w-3xl mx-auto">
              A practical, hands-on guide to building resilient, scalable, and observable event-driven systems using Go, Kotlin, and Ruby with Apache Kafka.
            </p>
            <div className="mt-10 max-w-sm mx-auto sm:max-w-none sm:flex sm:justify-center">
              <div className="space-y-4 sm:space-y-0 sm:mx-auto sm:inline-grid sm:grid-cols-2 sm:gap-5">
                <Link href="/about" className="flex items-center justify-center px-4 py-3 border border-transparent text-base font-medium rounded-md shadow-sm text-red-700 bg-white hover:bg-red-50 sm:px-8">
                  Learn More
                </Link>
                <Link href="/sample-chapters" className="flex items-center justify-center px-4 py-3 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-red-500 bg-opacity-60 hover:bg-opacity-70 sm:px-8">
                  Read Samples
                </Link>
              </div>
            </div>
          </div>
        </div>

        {/* Key Features Section */}
        <div className="py-16 bg-gray-50 overflow-hidden">
          <div className="max-w-7xl mx-auto px-4 space-y-8 sm:px-6 lg:px-8">
            <div className="text-center">
              <h2 className="text-base font-semibold text-red-600 tracking-wide uppercase">Features</h2>
              <p className="mt-2 text-3xl font-extrabold text-gray-900 tracking-tight sm:text-4xl">
                Why This Book?
              </p>
            </div>
            <div className="grid grid-cols-1 gap-y-10 sm:grid-cols-2 lg:grid-cols-3 gap-x-6">
              <div className="bg-white p-6 rounded-lg shadow-md">
                <h3 className="text-lg font-medium text-gray-900">Log-Centric Principles</h3>
                <p className="mt-2 text-base text-gray-500">
                  Understand the power of the distributed log and how it transforms system design.
                </p>
              </div>
              <div className="bg-white p-6 rounded-lg shadow-md">
                <h3 className="text-lg font-medium text-gray-900">Polyglot Implementation</h3>
                <p className="mt-2 text-base text-gray-500">
                  See practical Kafka integration examples in Go, Kotlin, and Ruby.
                </p>
              </div>
              <div className="bg-white p-6 rounded-lg shadow-md">
                <h3 className="text-lg font-medium text-gray-900">Event-Driven Patterns</h3>
                <p className="mt-2 text-base text-gray-500">
                  Learn essential patterns like Sagas, Event Sourcing, and CQRS in practice.
                </p>
              </div>
              <div className="bg-white p-6 rounded-lg shadow-md">
                <h3 className="text-lg font-medium text-gray-900">Kafka Essentials</h3>
                <p className="mt-2 text-base text-gray-500">
                  Master core Kafka concepts: topics, partitions, delivery guarantees, and more.
                </p>
              </div>
              <div className="bg-white p-6 rounded-lg shadow-md">
                <h3 className="text-lg font-medium text-gray-900">Hands-On Approach</h3>
                <p className="mt-2 text-base text-gray-500">
                  Follow along with a complete, working reference implementation.
                </p>
              </div>
              <div className="bg-white p-6 rounded-lg shadow-md">
                <h3 className="text-lg font-medium text-gray-900">Operational Insights</h3>
                <p className="mt-2 text-base text-gray-500">
                  Explore testing, monitoring, scaling, and security for production systems.
                </p>
              </div>
            </div>
          </div>
        </div>
      </main>
      <Footer />
    </div>
  );
}
