import { Toaster } from "@/components/ui/sonner";
import { TooltipProvider } from "@/components/ui/tooltip";
import NotFound from "@/pages/NotFound";
import { Route, Switch } from "wouter";
import ErrorBoundary from "./components/ErrorBoundary";
import { ThemeProvider } from "./contexts/ThemeContext";
import Home from "./pages/Home";
import Repository from "./pages/Repository";
import FileBrowser from "./pages/FileBrowser";
import FileView from "./pages/FileView";
import Reviews from "./pages/Reviews";
import ReviewDetail from "./pages/ReviewDetail";
import Quizzes from "./pages/Quizzes";
import QuizTake from "./pages/QuizTake";
import Guides from "./pages/Guides";
import GuideWalkthrough from "./pages/GuideWalkthrough";

function Router() {
  return (
    <Switch>
      <Route path="/" component={Home} />
      <Route path="/repo" component={Repository} />
      <Route path="/browse" component={FileBrowser} />
      <Route path="/browse/*" component={FileBrowser} />
      <Route path="/file/*" component={FileView} />
      <Route path="/reviews" component={Reviews} />
      <Route path="/review/:commit" component={ReviewDetail} />
      <Route path="/quizzes" component={Quizzes} />
      <Route path="/quiz/:commit" component={QuizTake} />
      <Route path="/guides" component={Guides} />
      <Route path="/guide/:commit" component={GuideWalkthrough} />
      <Route path="/guide/:commit/:stopId" component={GuideWalkthrough} />
      <Route path="/404" component={NotFound} />
      <Route component={NotFound} />
    </Switch>
  );
}

function App() {
  return (
    <ErrorBoundary>
      <ThemeProvider defaultTheme="dark">
        <TooltipProvider>
          <Toaster />
          <Router />
        </TooltipProvider>
      </ThemeProvider>
    </ErrorBoundary>
  );
}

export default App;
