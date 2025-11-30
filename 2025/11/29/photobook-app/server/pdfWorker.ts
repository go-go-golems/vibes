import { jsPDF } from "jspdf";
import { getPendingPdfJobs, updatePdfJob, getPdfJob } from "./db";
import { storagePut } from "./storage";
import { nanoid } from "nanoid";
import { getDb } from "./db";
import { eq } from "drizzle-orm";
import { photos } from "../drizzle/schema";
import { createCanvas, loadImage } from "canvas";
console.log("[PDF Worker] Canvas module loaded successfully!", typeof loadImage);

interface LogEntry {
  timestamp: string;
  level: "info" | "warn" | "error";
  message: string;
}

class PdfJobLogger {
  private logs: LogEntry[] = [];

  log(level: LogEntry["level"], message: string) {
    const entry: LogEntry = {
      timestamp: new Date().toISOString(),
      level,
      message,
    };
    this.logs.push(entry);
    console.log(`[PDF Worker] [${level.toUpperCase()}] ${message}`);
  }

  info(message: string) {
    this.log("info", message);
  }

  warn(message: string) {
    this.log("warn", message);
  }

  error(message: string) {
    this.log("error", message);
  }

  getLogs(): string {
    return JSON.stringify(this.logs);
  }
}

async function fetchImageAsDataUrl(url: string): Promise<string> {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Failed to fetch image: ${response.statusText}`);
  }
  
  const buffer = await response.arrayBuffer();
  const base64 = Buffer.from(buffer).toString('base64');
  const contentType = response.headers.get('content-type') || 'image/jpeg';
  
  return `data:${contentType};base64,${base64}`;
}

async function generatePdfForJob(jobId: number): Promise<void> {
  const logger = new PdfJobLogger();
  
  try {
    logger.info(`Starting PDF generation for job ${jobId}`);
    
    // Update job status to processing
    await updatePdfJob(jobId, {
      status: "processing",
      logs: logger.getLogs(),
    });
    
    // Get job details
    const job = await getPdfJob(jobId);
    if (!job) {
      throw new Error(`Job ${jobId} not found`);
    }
    
    logger.info(`Job found for user ${job.userId}`);
    
    // Parse photo IDs
    const photoIds: number[] = JSON.parse(job.photoIds);
    logger.info(`Processing ${photoIds.length} photos`);
    
    // Fetch photo records from database
    const db = await getDb();
    if (!db) throw new Error("Database not available");
    
    const photoRecords = await db
      .select()
      .from(photos)
      .where(eq(photos.userId, job.userId));
    
    // Filter and sort by the requested IDs
    const orderedPhotos = photoIds
      .map(id => photoRecords.find(p => p.id === id))
      .filter((p): p is NonNullable<typeof p> => p !== undefined);
    
    if (orderedPhotos.length === 0) {
      throw new Error("No valid photos found");
    }
    
    logger.info(`Found ${orderedPhotos.length} photo records in database`);
    
    // Create PDF
    const pdf = new jsPDF({
      orientation: "portrait",
      unit: "mm",
      format: "a4",
    });
    
    const pageWidth = pdf.internal.pageSize.getWidth();
    const pageHeight = pdf.internal.pageSize.getHeight();
    const margin = 10;
    const imageWidth = pageWidth - 2 * margin;
    const imageHeight = pageHeight - 2 * margin;
    
    for (let i = 0; i < orderedPhotos.length; i++) {
      const photo = orderedPhotos[i];
      logger.info(`Processing photo ${i + 1}/${orderedPhotos.length}: ${photo.filename}`);
      
      if (i > 0) {
        pdf.addPage();
      }
      
      try {
        // Fetch image directly as buffer
        logger.info(`Fetching image from ${photo.url}`);
        const response = await fetch(photo.url);
        if (!response.ok) {
          throw new Error(`Failed to fetch image: ${response.statusText}`);
        }
        const imageBuffer = Buffer.from(await response.arrayBuffer());
        
        // Load image using canvas to get dimensions
        logger.info(`Loading image with canvas to get dimensions`);
        const img = await loadImage(imageBuffer);
        const imgElement = { width: img.width, height: img.height };
        
        logger.info(`Image dimensions: ${imgElement.width}x${imgElement.height}`);
        
        const imgAspectRatio = imgElement.width / imgElement.height;
        const pageAspectRatio = imageWidth / imageHeight;
        
        let finalWidth = imageWidth;
        let finalHeight = imageHeight;
        let xOffset = margin;
        let yOffset = margin;
        
        if (imgAspectRatio > pageAspectRatio) {
          finalHeight = imageWidth / imgAspectRatio;
          yOffset = margin + (imageHeight - finalHeight) / 2;
        } else {
          finalWidth = imageHeight * imgAspectRatio;
          xOffset = margin + (imageWidth - finalWidth) / 2;
        }
        
        // Add image using buffer (Node.js compatible)
        pdf.addImage(
          imageBuffer,
          "JPEG",
          xOffset,
          yOffset,
          finalWidth,
          finalHeight
        );
        
        logger.info(`Successfully added photo ${i + 1} to PDF`);
        
        // Update progress
        await updatePdfJob(jobId, {
          logs: logger.getLogs(),
        });
      } catch (error) {
        logger.error(`Failed to process photo ${i + 1}: ${error instanceof Error ? error.message : String(error)}`);
        // Continue with next photo
      }
    }
    
    logger.info("Generating PDF buffer");
    const pdfBuffer = Buffer.from(pdf.output('arraybuffer'));
    
    // Upload PDF to S3
    logger.info("Uploading PDF to S3");
    const fileKey = `user-${job.userId}/pdfs/${nanoid()}-photobook.pdf`;
    const { url: pdfUrl } = await storagePut(fileKey, pdfBuffer, "application/pdf");
    
    logger.info(`PDF uploaded successfully: ${pdfUrl}`);
    
    // Update job as completed
    await updatePdfJob(jobId, {
      status: "completed",
      resultUrl: pdfUrl,
      logs: logger.getLogs(),
      completedAt: new Date(),
    });
    
    logger.info(`Job ${jobId} completed successfully`);
  } catch (error) {
    logger.error(`Job ${jobId} failed: ${error instanceof Error ? error.message : String(error)}`);
    
    await updatePdfJob(jobId, {
      status: "failed",
      errorMessage: error instanceof Error ? error.message : String(error),
      logs: logger.getLogs(),
    });
  }
}

/**
 * Process pending PDF jobs
 */
export async function processPdfJobs(): Promise<void> {
  const pendingJobs = await getPendingPdfJobs();
  
  console.log(`[PDF Worker] Found ${pendingJobs.length} pending jobs`);
  
  for (const job of pendingJobs) {
    await generatePdfForJob(job.id);
  }
}

// Run worker every 10 seconds
let workerInterval: NodeJS.Timeout | null = null;

export function startPdfWorker() {
  if (workerInterval) {
    console.log("[PDF Worker] Worker already running");
    return;
  }
  
  console.log("[PDF Worker] Starting PDF worker");
  workerInterval = setInterval(() => {
    processPdfJobs().catch(error => {
      console.error("[PDF Worker] Error processing jobs:", error);
    });
  }, 10000); // Every 10 seconds
  
  // Process immediately on start
  processPdfJobs().catch(error => {
    console.error("[PDF Worker] Error processing jobs:", error);
  });
}

export function stopPdfWorker() {
  if (workerInterval) {
    clearInterval(workerInterval);
    workerInterval = null;
    console.log("[PDF Worker] Worker stopped");
  }
}
