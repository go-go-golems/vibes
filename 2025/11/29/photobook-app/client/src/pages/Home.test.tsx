import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, fireEvent, waitFor } from "@testing-library/react";
import "@testing-library/jest-dom";
import Home from "./Home";

// Mock sonner toast
vi.mock("sonner", () => ({
  toast: {
    success: vi.fn(),
    error: vi.fn(),
  },
}));

describe("Home Component", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("renders empty state when no images are uploaded", () => {
    render(<Home />);
    
    expect(screen.getByText("No Images Yet")).toBeInTheDocument();
    expect(screen.getByText(/Upload a folder of images to start creating your photobook/i)).toBeInTheDocument();
  });

  it("renders upload button in header", () => {
    render(<Home />);
    
    const uploadButtons = screen.getAllByText(/Upload/i);
    expect(uploadButtons.length).toBeGreaterThan(0);
  });

  it("shows file input when upload button is clicked", () => {
    render(<Home />);
    
    const fileInput = document.querySelector('input[type="file"]') as HTMLInputElement;
    expect(fileInput).toBeInTheDocument();
    expect(fileInput.accept).toBe("image/*");
    expect(fileInput.multiple).toBe(true);
  });

  it("handles file upload correctly", async () => {
    const { toast } = await import("sonner");
    render(<Home />);
    
    const fileInput = document.querySelector('input[type="file"]') as HTMLInputElement;
    
    // Create mock files
    const file1 = new File(["image1"], "test1.jpg", { type: "image/jpeg" });
    const file2 = new File(["image2"], "test2.png", { type: "image/png" });
    
    // Mock URL.createObjectURL
    global.URL.createObjectURL = vi.fn(() => "blob:mock-url");
    
    // Simulate file upload
    Object.defineProperty(fileInput, "files", {
      value: [file1, file2],
      writable: false,
    });
    
    fireEvent.change(fileInput);
    
    await waitFor(() => {
      expect(toast.success).toHaveBeenCalledWith("Added 2 images");
    });
  });

  it("displays Clear All button when images are present", async () => {
    render(<Home />);
    
    const fileInput = document.querySelector('input[type="file"]') as HTMLInputElement;
    const file = new File(["image"], "test.jpg", { type: "image/jpeg" });
    
    global.URL.createObjectURL = vi.fn(() => "blob:mock-url");
    
    Object.defineProperty(fileInput, "files", {
      value: [file],
      writable: false,
    });
    
    fireEvent.change(fileInput);
    
    await waitFor(() => {
      expect(screen.getByText("Clear All")).toBeInTheDocument();
    });
  });

  it("component mounts without errors", () => {
    expect(() => render(<Home />)).not.toThrow();
  });
});
