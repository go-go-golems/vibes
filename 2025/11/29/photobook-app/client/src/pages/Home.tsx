import { useState, useRef } from "react";
import { Button } from "@/components/ui/button";
import { Upload, Play, Pause, ChevronLeft, ChevronRight, Trash2, X } from "lucide-react";
import { toast } from "sonner";
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
  id: string;
  url: string;
  name: string;
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
  const [images, setImages] = useState<ImageFile[]>([]);
  const [selectedIndex, setSelectedIndex] = useState(0);
  const [isPlaying, setIsPlaying] = useState(false);
  const fileInputRef = useRef<HTMLInputElement>(null);
  const slideshowIntervalRef = useRef<NodeJS.Timeout | null>(null);

  const sensors = useSensors(
    useSensor(PointerSensor),
    useSensor(KeyboardSensor, {
      coordinateGetter: sortableKeyboardCoordinates,
    })
  );

  const handleFileUpload = (event: React.ChangeEvent<HTMLInputElement>) => {
    const files = event.target.files;
    if (!files) return;

    const newImages: ImageFile[] = [];
    Array.from(files).forEach((file) => {
      if (file.type.startsWith("image/")) {
        const url = URL.createObjectURL(file);
        newImages.push({
          id: `${Date.now()}-${Math.random()}`,
          url,
          name: file.name,
        });
      }
    });

    if (newImages.length > 0) {
      setImages((prev) => [...prev, ...newImages]);
      toast.success(`Added ${newImages.length} image${newImages.length > 1 ? "s" : ""}`);
    } else {
      toast.error("No valid image files found");
    }
  };

  const handleDragEnd = (event: DragEndEvent) => {
    const { active, over } = event;

    if (over && active.id !== over.id) {
      setImages((items) => {
        const oldIndex = items.findIndex((item) => item.id === active.id);
        const newIndex = items.findIndex((item) => item.id === over.id);

        // Update selected index if the selected image was moved
        if (oldIndex === selectedIndex) {
          setSelectedIndex(newIndex);
        } else if (oldIndex < selectedIndex && newIndex >= selectedIndex) {
          setSelectedIndex(selectedIndex - 1);
        } else if (oldIndex > selectedIndex && newIndex <= selectedIndex) {
          setSelectedIndex(selectedIndex + 1);
        }

        return arrayMove(items, oldIndex, newIndex);
      });
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

  const handleDeleteImage = (id: string) => {
    const index = images.findIndex((img) => img.id === id);
    setImages((prev) => prev.filter((img) => img.id !== id));
    
    if (index === selectedIndex && selectedIndex >= images.length - 1) {
      setSelectedIndex(Math.max(0, images.length - 2));
    } else if (index < selectedIndex) {
      setSelectedIndex(selectedIndex - 1);
    }
  };

  const handleClearAll = () => {
    if (slideshowIntervalRef.current) {
      clearInterval(slideshowIntervalRef.current);
      slideshowIntervalRef.current = null;
    }
    setImages([]);
    setSelectedIndex(0);
    setIsPlaying(false);
    toast.success("All images cleared");
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
            />
            <Button onClick={() => fileInputRef.current?.click()} variant="default" size="sm" className="md:h-10">
              <Upload className="w-4 h-4 mr-2" />
              Upload Images
            </Button>
            {images.length > 0 && (
              <Button onClick={handleClearAll} variant="destructive" size="sm" className="md:h-10">
                <Trash2 className="w-4 h-4 mr-2" />
                Clear All
              </Button>
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
              <Button onClick={() => fileInputRef.current?.click()} size="lg" variant="default">
                <Upload className="w-5 h-5 mr-2" />
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
