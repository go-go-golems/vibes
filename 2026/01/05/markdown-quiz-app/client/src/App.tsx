import { Toaster } from "@/components/ui/sonner";
import { TooltipProvider } from "@/components/ui/tooltip";
import NotFound from "@/pages/NotFound";
import { Route, Switch } from "wouter";
import ErrorBoundary from "./components/ErrorBoundary";
import { ThemeProvider } from "./contexts/ThemeContext";
import Home from "./pages/Home";
import Admin from "./pages/Admin";
import DocumentEditor from "./pages/DocumentEditor";
import DocumentView from "./pages/DocumentView";
import Analytics from "./pages/Analytics";
import MySubmissions from "./pages/MySubmissions";
import SubmissionReview from "./pages/SubmissionReview";

function Router() {
  return (
    <Switch>
      <Route path="/" component={Home} />
      <Route path="/admin" component={Admin} />
      <Route path="/admin/new" component={DocumentEditor} />
      <Route path="/admin/edit/:id" component={DocumentEditor} />
      <Route path="/admin/analytics/:id" component={Analytics} />
      <Route path="/admin/submissions" component={MySubmissions} />
      <Route path="/documents/:slug" component={DocumentView} />
      <Route path="/submission/:id" component={SubmissionReview} />
      <Route path="/404" component={NotFound} />
      <Route component={NotFound} />
    </Switch>
  );
}

function App() {
  return (
    <ErrorBoundary>
      <ThemeProvider defaultTheme="light">
        <TooltipProvider>
          <Toaster />
          <Router />
        </TooltipProvider>
      </ThemeProvider>
    </ErrorBoundary>
  );
}

export default App;
