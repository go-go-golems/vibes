import { useState, useRef, useEffect } from "react";
import { Button } from "@/components/ui/button";
import { Upload, Play, Pause, ChevronLeft, ChevronRight, Trash2, X, ArrowUp, ArrowDown, Shuffle, Download, Loader2 } from "lucide-react";
import { toast } from "sonner";
import { trpc } from "@/lib/trpc";
import { useAuth } from "@/_core/hooks/useAuth";
import { getLoginUrl } from "@/const";
import {
  DndContext,
  closestCenter,
  KeyboardSensor,
  PointerSensor,
  useSensor,
  useSensors,
  DragEndEvent,
} from "@dnd-kit/core";
import {
  arrayMove,
  SortableContext,
  sortableKeyboardCoordinates,
  verticalListSortingStrategy,
  useSortable,
} from "@dnd-kit/sortable";
import { CSS } from "@dnd-kit/utilities";

interface ImageFile {
  id: number;
  url: string;
  name: string;
  position: number;
}

function SortableThumbnail({ image, isSelected, onClick, onDelete }: {
  image: ImageFile;
  isSelected: boolean;
  onClick: () => void;
  onDelete: () => void;
}) {
  const {
    attributes,
    listeners,
    setNodeRef,
    transform,
    transition,
    isDragging,
  } = useSortable({ id: image.id });

  const style = {
    transform: CSS.Transform.toString(transform),
    transition,
    opacity: isDragging ? 0.5 : 1,
  };

  return (
    <div
      ref={setNodeRef}
      style={style}
      className={`relative group cursor-pointer rounded-lg overflow-hidden border-2 transition-all flex-shrink-0 w-32 md:w-full ${
        isSelected ? "border-primary ring-2 ring-primary/50" : "border-border hover:border-primary/50"
      }`}
      {...attributes}
      {...listeners}
    >
      <div onClick={onClick} className="aspect-square w-full">
        <img
          src={image.url}
          alt={image.name}
          className="w-full h-full object-cover"
        />
      </div>
      <button
        onClick={(e) => {
          e.stopPropagation();
          onDelete();
        }}
        className="absolute top-1 right-1 bg-destructive/90 hover:bg-destructive text-destructive-foreground rounded-full p-1 opacity-0 group-hover:opacity-100 transition-opacity"
      >
        <X className="w-3 h-3" />
      </button>
    </div>
  );
}

export default function Home() {
  const { user, loading: authLoading } = useAuth();
  const [selectedIndex, setSelectedIndex] = useState(0);
  const [isPlaying, setIsPlaying] = useState(false);
  const fileInputRef = useRef<HTMLInputElement>(null);
  const slideshowIntervalRef = useRef<NodeJS.Timeout | null>(null);
  const [isUploading, setIsUploading] = useState(false);

  // Fetch photos from server
  const { data: photos = [], isLoading, refetch } = trpc.photo.list.useQuery(undefined, {
    enabled: !!user,
  });

  // Mutations
  const uploadMutation = trpc.photo.upload.useMutation();
  const updatePositionsMutation = trpc.photo.updatePositions.useMutation();
  const deleteMutation = trpc.photo.delete.useMutation();
  const deleteAllMutation = trpc.photo.deleteAll.useMutation();
  const createPdfJobMutation = trpc.pdf.createJob.useMutation();

  const images: ImageFile[] = photos.map(p => ({
    id: p.id,
    url: p.url,
    name: p.filename,
    position: p.position,
  }));

  const sensors = useSensors(
    useSensor(PointerSensor),
    useSensor(KeyboardSensor, {
      coordinateGetter: sortableKeyboardCoordinates,
    })
  );

  const handleFileUpload = async (event: React.ChangeEvent<HTMLInputElement>) => {
    const files = event.target.files;
    if (!files || !user) return;

    setIsUploading(true);
    const toastId = toast.loading("Uploading images...");

    try {
      let uploadedCount = 0;
      const currentMaxPosition = Math.max(0, ...photos.map(p => p.position));

      for (let i = 0; i < files.length; i++) {
        const file = files[i];
        if (!file.type.startsWith("image/")) continue;

        // Convert file to base64
        const reader = new FileReader();
        const base64Data = await new Promise<string>((resolve, reject) => {
          reader.onload = () => {
            const result = reader.result as string;
            const base64 = result.split(',')[1];
            resolve(base64);
          };
          reader.onerror = reject;
          reader.readAsDataURL(file);
        });

        await uploadMutation.mutateAsync({
          filename: file.name,
          mimeType: file.type,
          data: base64Data,
          position: currentMaxPosition + i + 1,
        });

        uploadedCount++;
        toast.loading(`Uploading ${uploadedCount}/${files.length}...`, { id: toastId });
      }

      await refetch();
      toast.success(`Uploaded ${uploadedCount} image${uploadedCount > 1 ? "s" : ""}`, { id: toastId });
    } catch (error) {
      console.error("Upload error:", error);
      toast.error("Failed to upload images", { id: toastId });
    } finally {
      setIsUploading(false);
    }
  };

  const handleDragEnd = async (event: DragEndEvent) => {
    const { active, over } = event;

    if (over && active.id !== over.id) {
      const oldIndex = images.findIndex((item) => item.id === active.id);
      const newIndex = images.findIndex((item) => item.id === over.id);

      const reordered = arrayMove(images, oldIndex, newIndex);

      // Update positions
      const updates = reordered.map((img, idx) => ({
        id: img.id,
        position: idx,
      }));

      try {
        await updatePositionsMutation.mutateAsync({ updates });
        await refetch();

        // Update selected index
        if (oldIndex === selectedIndex) {
          setSelectedIndex(newIndex);
        } else if (oldIndex < selectedIndex && newIndex >= selectedIndex) {
          setSelectedIndex(selectedIndex - 1);
        } else if (oldIndex > selectedIndex && newIndex <= selectedIndex) {
          setSelectedIndex(selectedIndex + 1);
        }
      } catch (error) {
        console.error("Reorder error:", error);
        toast.error("Failed to reorder images");
      }
    }
  };

  const handlePrevious = () => {
    setSelectedIndex((prev) => (prev > 0 ? prev - 1 : images.length - 1));
  };

  const handleNext = () => {
    setSelectedIndex((prev) => (prev < images.length - 1 ? prev + 1 : 0));
  };

  const toggleSlideshow = () => {
    if (isPlaying) {
      if (slideshowIntervalRef.current) {
        clearInterval(slideshowIntervalRef.current);
        slideshowIntervalRef.current = null;
      }
      setIsPlaying(false);
    } else {
      setIsPlaying(true);
      slideshowIntervalRef.current = setInterval(() => {
        setSelectedIndex((prev) => (prev < images.length - 1 ? prev + 1 : 0));
      }, 3000);
    }
  };

  const handleDeleteImage = async (id: number) => {
    const index = images.findIndex((img) => img.id === id);
    
    try {
      await deleteMutation.mutateAsync({ id });
      await refetch();
      
      if (index === selectedIndex && selectedIndex >= images.length - 1) {
        setSelectedIndex(Math.max(0, images.length - 2));
      } else if (index < selectedIndex) {
        setSelectedIndex(selectedIndex - 1);
      }
    } catch (error) {
      console.error("Delete error:", error);
      toast.error("Failed to delete image");
    }
  };

  const handleClearAll = async () => {
    if (slideshowIntervalRef.current) {
      clearInterval(slideshowIntervalRef.current);
      slideshowIntervalRef.current = null;
    }

    try {
      await deleteAllMutation.mutateAsync();
      await refetch();
      setSelectedIndex(0);
      setIsPlaying(false);
      toast.success("All images cleared");
    } catch (error) {
      console.error("Clear all error:", error);
      toast.error("Failed to clear images");
    }
  };

  const handleRandomize = async () => {
    const shuffled = [...images].sort(() => Math.random() - 0.5);
    const updates = shuffled.map((img, idx) => ({
      id: img.id,
      position: idx,
    }));

    try {
      await updatePositionsMutation.mutateAsync({ updates });
      await refetch();
      setSelectedIndex(0);
      toast.success("Images randomized");
    } catch (error) {
      console.error("Randomize error:", error);
      toast.error("Failed to randomize images");
    }
  };

  const handleMoveUp = async (index: number) => {
    if (index === 0) return;
    const newImages = [...images];
    [newImages[index - 1], newImages[index]] = [newImages[index], newImages[index - 1]];
    
    const updates = newImages.map((img, idx) => ({
      id: img.id,
      position: idx,
    }));

    try {
      await updatePositionsMutation.mutateAsync({ updates });
      await refetch();
      
      if (selectedIndex === index) {
        setSelectedIndex(index - 1);
      } else if (selectedIndex === index - 1) {
        setSelectedIndex(index);
      }
    } catch (error) {
      console.error("Move up error:", error);
      toast.error("Failed to move image");
    }
  };

  const handleMoveDown = async (index: number) => {
    if (index === images.length - 1) return;
    const newImages = [...images];
    [newImages[index], newImages[index + 1]] = [newImages[index + 1], newImages[index]];
    
    const updates = newImages.map((img, idx) => ({
      id: img.id,
      position: idx,
    }));

    try {
      await updatePositionsMutation.mutateAsync({ updates });
      await refetch();
      
      if (selectedIndex === index) {
        setSelectedIndex(index + 1);
      } else if (selectedIndex === index + 1) {
        setSelectedIndex(index);
      }
    } catch (error) {
      console.error("Move down error:", error);
      toast.error("Failed to move image");
    }
  };

  const handleDownloadPDF = async () => {
    if (images.length === 0) {
      toast.error("No images to download");
      return;
    }

    const toastId = toast.loading("Creating PDF generation job...");

    try {
      const photoIds = images.map(img => img.id);
      const result = await createPdfJobMutation.mutateAsync({ photoIds });
      
      toast.success(result.message, { id: toastId });
      toast.info("You can check the PDF status in the jobs list. The PDF will be ready shortly.");
    } catch (error) {
      console.error("PDF job error:", error);
      toast.error("Failed to create PDF job", { id: toastId });
    }
  };

  // Keyboard navigation
  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (images.length === 0) return;
    
    if (e.key === "ArrowLeft") {
      handlePrevious();
    } else if (e.key === "ArrowRight") {
      handleNext();
    } else if (e.key === " ") {
      e.preventDefault();
      toggleSlideshow();
    }
  };

  // Show login prompt if not authenticated
  if (!authLoading && !user) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-background">
        <div className="text-center max-w-md">
          <h2 className="text-2xl font-semibold mb-4 text-foreground">Please Log In</h2>
          <p className="text-muted-foreground mb-6">
            You need to be logged in to use the Photobook Creator.
          </p>
          <Button onClick={() => window.location.href = getLoginUrl()} size="lg">
            Log In
          </Button>
        </div>
      </div>
    );
  }

  // Show loading state
  if (authLoading || isLoading) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-background">
        <Loader2 className="w-8 h-8 animate-spin text-primary" />
      </div>
    );
  }

  return (
    <div className="min-h-screen flex flex-col bg-background text-foreground overflow-hidden" onKeyDown={handleKeyDown} tabIndex={0}>
      {/* Header */}
      <header className="border-b border-border bg-card">
        <div className="container py-3 md:py-4 flex items-center justify-between">
          <h1 className="text-lg md:text-2xl font-bold text-foreground">Photobook Creator</h1>
          <div className="flex gap-2">
            <input
              ref={fileInputRef}
              type="file"
              accept="image/*"
              multiple
              onChange={handleFileUpload}
              className="hidden"
              disabled={isUploading}
            />
            <Button 
              onClick={() => fileInputRef.current?.click()} 
              variant="default" 
              size="sm" 
              className="md:h-10"
              disabled={isUploading}
            >
              {isUploading ? <Loader2 className="w-4 h-4 mr-2 animate-spin" /> : <Upload className="w-4 h-4 mr-2" />}
              Upload Images
            </Button>
            {images.length > 0 && (
              <>
                <Button onClick={handleDownloadPDF} variant="outline" size="sm" className="md:h-10">
                  <Download className="w-4 h-4 mr-2" />
                  <span className="hidden sm:inline">PDF</span>
                </Button>
                <Button onClick={handleRandomize} variant="outline" size="sm" className="md:h-10">
                  <Shuffle className="w-4 h-4 mr-2" />
                  <span className="hidden sm:inline">Randomize</span>
                </Button>
                <Button onClick={handleClearAll} variant="destructive" size="sm" className="md:h-10">
                  <Trash2 className="w-4 h-4 mr-2" />
                  <span className="hidden sm:inline">Clear All</span>
                </Button>
              </>
            )}
          </div>
        </div>
      </header>

      {/* Main Content */}
      <main className="flex-1 flex flex-col md:flex-row overflow-hidden">
        {images.length === 0 ? (
          <div className="flex-1 flex items-center justify-center">
            <div className="text-center max-w-md">
              <Upload className="w-16 h-16 mx-auto mb-4 text-muted-foreground" />
              <h2 className="text-2xl font-semibold mb-2 text-foreground">No Images Yet</h2>
              <p className="text-muted-foreground mb-6">
                Upload a folder of images to start creating your photobook. You can reorder them and view them in a beautiful slideshow.
              </p>
              <Button onClick={() => fileInputRef.current?.click()} size="lg" variant="default" disabled={isUploading}>
                {isUploading ? <Loader2 className="w-5 h-5 mr-2 animate-spin" /> : <Upload className="w-5 h-5 mr-2" />}
                Upload Your First Images
              </Button>
            </div>
          </div>
        ) : (
          <>
            {/* Left Sidebar - Thumbnails */}
            <aside className="w-full md:w-64 border-b md:border-b-0 md:border-r border-border bg-sidebar overflow-y-auto p-4 max-h-48 md:max-h-none">
              <div className="flex items-center justify-between mb-4">
                <h2 className="text-sm font-semibold text-sidebar-foreground">Images ({images.length})</h2>
              </div>
              <DndContext
                sensors={sensors}
                collisionDetection={closestCenter}
                onDragEnd={handleDragEnd}
              >
                <SortableContext
                  items={images.map((img) => img.id)}
                  strategy={verticalListSortingStrategy}
                >
                  <div className="space-y-3 md:space-y-3 flex md:block gap-3 md:gap-0 overflow-x-auto md:overflow-x-visible pb-2 md:pb-0">
                    {images.map((image, index) => (
                      <SortableThumbnail
                        key={image.id}
                        image={image}
                        isSelected={index === selectedIndex}
                        onClick={() => setSelectedIndex(index)}
                        onDelete={() => handleDeleteImage(image.id)}
                      />
                    ))}
                  </div>
                </SortableContext>
              </DndContext>
            </aside>

            {/* Right Side - Preview */}
            <div className="flex-1 flex flex-col">
              {/* Reorder Controls */}
              <div className="border-b border-border bg-card p-3">
                <div className="container flex items-center justify-center gap-3">
                  <Button 
                    onClick={() => handleMoveUp(selectedIndex)} 
                    variant="outline" 
                    size="lg"
                    disabled={selectedIndex === 0}
                    className="h-12 px-6"
                  >
                    <ArrowUp className="w-5 h-5 mr-2" />
                    Move Up
                  </Button>
                  <div className="text-sm text-muted-foreground min-w-[100px] text-center">
                    Position {selectedIndex + 1} of {images.length}
                  </div>
                  <Button 
                    onClick={() => handleMoveDown(selectedIndex)} 
                    variant="outline" 
                    size="lg"
                    disabled={selectedIndex === images.length - 1}
                    className="h-12 px-6"
                  >
                    <ArrowDown className="w-5 h-5 mr-2" />
                    Move Down
                  </Button>
                </div>
              </div>
              {/* Preview Area */}
              <div className="flex-1 flex items-center justify-center p-8 bg-background">
                <div className="relative max-w-5xl w-full h-full flex items-center justify-center">
                  <img
                    src={images[selectedIndex]?.url}
                    alt={images[selectedIndex]?.name}
                    className="max-w-full max-h-full object-contain rounded-lg shadow-2xl"
                  />
                </div>
              </div>

              {/* Controls */}
              <div className="border-t border-border bg-card p-4 md:p-6">
                <div className="container flex flex-col md:flex-row items-center justify-between gap-4 md:gap-0">
                  <div className="flex items-center gap-4">
                    <Button onClick={handlePrevious} variant="outline" size="icon">
                      <ChevronLeft className="w-5 h-5" />
                    </Button>
                    <Button onClick={toggleSlideshow} variant="default" size="icon">
                      {isPlaying ? <Pause className="w-5 h-5" /> : <Play className="w-5 h-5" />}
                    </Button>
                    <Button onClick={handleNext} variant="outline" size="icon">
                      <ChevronRight className="w-5 h-5" />
                    </Button>
                  </div>
                  <div className="text-sm text-muted-foreground">
                    {selectedIndex + 1} / {images.length}
                  </div>
                  <div className="text-xs md:text-sm text-muted-foreground hidden md:block">
                    Use arrow keys to navigate • Space to play/pause
                  </div>
                </div>
              </div>
            </div>
          </>
        )}
      </main>
    </div>
  );
}
