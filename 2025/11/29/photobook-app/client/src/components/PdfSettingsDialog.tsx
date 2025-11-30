import { useState } from "react";
import { Dialog, DialogContent, DialogDescription, DialogFooter, DialogHeader, DialogTitle } from "@/components/ui/dialog";
import { Button } from "@/components/ui/button";
import { Label } from "@/components/ui/label";
import { Input } from "@/components/ui/input";
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select";

export interface PdfSettings {
  pageWidth: number;
  pageHeight: number;
  marginTop: number;
  marginRight: number;
  marginBottom: number;
  marginLeft: number;
  preset?: string;
}

interface PdfSettingsDialogProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  onConfirm: (settings: PdfSettings) => void;
}

// Common photo format presets (in mm)
const PRESETS = {
  "4x6-portrait": { width: 101.6, height: 152.4, label: "4×6 Portrait" },
  "4x6-landscape": { width: 152.4, height: 101.6, label: "4×6 Landscape" },
  "5x7-portrait": { width: 127, height: 177.8, label: "5×7 Portrait" },
  "5x7-landscape": { width: 177.8, height: 127, label: "5×7 Landscape" },
  "8x10-portrait": { width: 203.2, height: 254, label: "8×10 Portrait" },
  "8x10-landscape": { width: 254, height: 203.2, label: "8×10 Landscape" },
  "a4-portrait": { width: 210, height: 297, label: "A4 Portrait" },
  "a4-landscape": { width: 297, height: 210, label: "A4 Landscape" },
  "letter-portrait": { width: 215.9, height: 279.4, label: "Letter Portrait" },
  "letter-landscape": { width: 279.4, height: 215.9, label: "Letter Landscape" },
  "custom": { width: 210, height: 297, label: "Custom" },
};

export function PdfSettingsDialog({ open, onOpenChange, onConfirm }: PdfSettingsDialogProps) {
  const [preset, setPreset] = useState<string>("a4-portrait");
  const [pageWidth, setPageWidth] = useState(210);
  const [pageHeight, setPageHeight] = useState(297);
  const [marginTop, setMarginTop] = useState(10);
  const [marginRight, setMarginRight] = useState(10);
  const [marginBottom, setMarginBottom] = useState(10);
  const [marginLeft, setMarginLeft] = useState(10);

  const handlePresetChange = (value: string) => {
    setPreset(value);
    if (value !== "custom") {
      const presetData = PRESETS[value as keyof typeof PRESETS];
      setPageWidth(presetData.width);
      setPageHeight(presetData.height);
    }
  };

  const handleConfirm = () => {
    onConfirm({
      pageWidth,
      pageHeight,
      marginTop,
      marginRight,
      marginBottom,
      marginLeft,
      preset: preset !== "custom" ? preset : undefined,
    });
    onOpenChange(false);
  };

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="sm:max-w-[500px] bg-background text-foreground">
        <DialogHeader>
          <DialogTitle>PDF Settings</DialogTitle>
          <DialogDescription>
            Customize your photobook PDF layout and dimensions
          </DialogDescription>
        </DialogHeader>
        
        <div className="grid gap-4 py-4">
          {/* Preset Selector */}
          <div className="grid gap-2">
            <Label htmlFor="preset">Format Preset</Label>
            <Select value={preset} onValueChange={handlePresetChange}>
              <SelectTrigger id="preset">
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                {Object.entries(PRESETS).map(([key, { label }]) => (
                  <SelectItem key={key} value={key}>
                    {label}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          {/* Page Dimensions */}
          <div className="grid grid-cols-2 gap-4">
            <div className="grid gap-2">
              <Label htmlFor="pageWidth">Width (mm)</Label>
              <Input
                id="pageWidth"
                type="number"
                value={pageWidth}
                onChange={(e) => setPageWidth(Number(e.target.value))}
                disabled={preset !== "custom"}
              />
            </div>
            <div className="grid gap-2">
              <Label htmlFor="pageHeight">Height (mm)</Label>
              <Input
                id="pageHeight"
                type="number"
                value={pageHeight}
                onChange={(e) => setPageHeight(Number(e.target.value))}
                disabled={preset !== "custom"}
              />
            </div>
          </div>

          {/* Margins */}
          <div className="grid gap-2">
            <Label>Margins (mm)</Label>
            <div className="grid grid-cols-2 gap-2">
              <Input
                placeholder="Top"
                type="number"
                value={marginTop}
                onChange={(e) => setMarginTop(Number(e.target.value))}
              />
              <Input
                placeholder="Right"
                type="number"
                value={marginRight}
                onChange={(e) => setMarginRight(Number(e.target.value))}
              />
              <Input
                placeholder="Bottom"
                type="number"
                value={marginBottom}
                onChange={(e) => setMarginBottom(Number(e.target.value))}
              />
              <Input
                placeholder="Left"
                type="number"
                value={marginLeft}
                onChange={(e) => setMarginLeft(Number(e.target.value))}
              />
            </div>
          </div>
        </div>

        <DialogFooter>
          <Button variant="outline" onClick={() => onOpenChange(false)}>
            Cancel
          </Button>
          <Button onClick={handleConfirm}>
            Generate PDF
          </Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  );
}
