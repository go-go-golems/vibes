package documentation

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"sync"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/redis/go-redis/v9"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	pb "github.com/drone-workflow-platform/github.com/drone-workflow-platform/pkg/models"
)

type Service struct {
	pb.UnimplementedDocumentationServiceServer
	documents map[string]*pb.Document
	redisClient *redis.Client
	logger watermill.LoggerAdapter
	mu sync.RWMutex
}

func NewService(redisClient *redis.Client, logger watermill.LoggerAdapter) *Service {
	service := &Service{
		documents: make(map[string]*pb.Document),
		redisClient: redisClient,
		logger: logger,
	}
	
	// Initialize with sample documents
	service.initializeSampleDocuments()
	
	return service
}

func (s *Service) GetDocument(ctx context.Context, req *pb.GetDocumentRequest) (*pb.GetDocumentResponse, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	document, exists := s.documents[req.DocumentId]
	if !exists {
		// Try to load from Redis
		docData, err := s.redisClient.Get(ctx, fmt.Sprintf("document:%s", req.DocumentId)).Result()
		if err != nil {
			return &pb.GetDocumentResponse{
				Success: false,
				Message: "document not found",
			}, status.Error(codes.NotFound, "document not found")
		}

		document = &pb.Document{}
		err = json.Unmarshal([]byte(docData), document)
		if err != nil {
			return &pb.GetDocumentResponse{
				Success: false,
				Message: "failed to deserialize document",
			}, status.Error(codes.Internal, "failed to deserialize document")
		}

		s.documents[req.DocumentId] = document
	}

	// Check access permissions
	if !s.hasAccess(req.UserClearance, document.ClearanceLevels) {
		s.logger.Warn("Access denied to document", watermill.LogFields{
			"document_id": req.DocumentId,
			"user_clearance": req.UserClearance,
			"required_clearance": strings.Join(document.ClearanceLevels, ","),
		})
		
		return &pb.GetDocumentResponse{
			Success: false,
			Message: "access denied - insufficient clearance level",
		}, status.Error(codes.PermissionDenied, "access denied")
	}

	s.logger.Info("Document accessed", watermill.LogFields{
		"document_id": req.DocumentId,
		"user_clearance": req.UserClearance,
		"title": document.Title,
	})

	return &pb.GetDocumentResponse{
		Document: document,
		Success: true,
		Message: "document retrieved successfully",
	}, nil
}

func (s *Service) ValidateDocumentAccess(ctx context.Context, req *pb.ValidateDocumentAccessRequest) (*pb.ValidateDocumentAccessResponse, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	document, exists := s.documents[req.DocumentId]
	if !exists {
		// Try to load from Redis
		docData, err := s.redisClient.Get(ctx, fmt.Sprintf("document:%s", req.DocumentId)).Result()
		if err != nil {
			return &pb.ValidateDocumentAccessResponse{
				AccessGranted: false,
				Message: "document not found",
			}, nil
		}

		document = &pb.Document{}
		err = json.Unmarshal([]byte(docData), document)
		if err != nil {
			return &pb.ValidateDocumentAccessResponse{
				AccessGranted: false,
				Message: "failed to deserialize document",
			}, nil
		}

		s.documents[req.DocumentId] = document
	}

	// Check clearance level access
	hasBasicAccess := s.hasAccess(req.UserClearance, document.ClearanceLevels)
	if !hasBasicAccess {
		return &pb.ValidateDocumentAccessResponse{
			AccessGranted: false,
			Message: "insufficient clearance level",
		}, nil
	}

	// Check additional access requirements
	if req.RequiredAccess != nil {
		// Check if digital signature is required and user has appropriate clearance
		if req.RequiredAccess.DigitalSignature && !s.canDigitallySign(req.UserClearance) {
			return &pb.ValidateDocumentAccessResponse{
				AccessGranted: false,
				Message: "digital signature required but user lacks signing privileges",
			}, nil
		}

		// Check if print access is required
		if req.RequiredAccess.PrintRequired && !s.canPrint(req.UserClearance, document.ClearanceLevels) {
			return &pb.ValidateDocumentAccessResponse{
				AccessGranted: false,
				Message: "print access required but not authorized",
			}, nil
		}

		// Check verification requirements
		if req.RequiredAccess.Verification && !s.canVerify(req.UserClearance) {
			return &pb.ValidateDocumentAccessResponse{
				AccessGranted: false,
				Message: "document verification required but user lacks verification privileges",
			}, nil
		}
	}

	s.logger.Info("Document access validated", watermill.LogFields{
		"document_id": req.DocumentId,
		"user_clearance": req.UserClearance,
		"access_granted": true,
	})

	return &pb.ValidateDocumentAccessResponse{
		AccessGranted: true,
		Message: "access granted",
	}, nil
}

func (s *Service) hasAccess(userClearance string, requiredClearances []string) bool {
	if len(requiredClearances) == 0 {
		return true // No specific clearance required
	}

	// Define clearance hierarchy
	clearanceHierarchy := map[string]int{
		"public": 0,
		"internal": 1,
		"restricted": 2,
		"confidential": 3,
	}

	userLevel, userExists := clearanceHierarchy[userClearance]
	if !userExists {
		return false
	}

	// Check if user has sufficient clearance for any of the required levels
	for _, required := range requiredClearances {
		requiredLevel, requiredExists := clearanceHierarchy[required]
		if requiredExists && userLevel >= requiredLevel {
			return true
		}
	}

	return false
}

func (s *Service) canDigitallySign(userClearance string) bool {
	// Only internal and above can digitally sign
	return userClearance == "internal" || userClearance == "restricted" || userClearance == "confidential"
}

func (s *Service) canPrint(userClearance string, documentClearances []string) bool {
	// Printing restrictions based on document sensitivity
	for _, clearance := range documentClearances {
		if clearance == "confidential" {
			return userClearance == "confidential"
		}
		if clearance == "restricted" {
			return userClearance == "restricted" || userClearance == "confidential"
		}
	}
	return true
}

func (s *Service) canVerify(userClearance string) bool {
	// Only restricted and confidential users can verify documents
	return userClearance == "restricted" || userClearance == "confidential"
}

func (s *Service) initializeSampleDocuments() {
	sampleDocs := []*pb.Document{
		{
			Id: "iso_9001",
			Title: "Quality Management Systems - ISO 9001:2015",
			Content: "This document outlines the requirements for a quality management system...",
			Url: "https://docs.company.com/standards/iso9001-2015.pdf",
			Version: "2015",
			ClearanceLevels: []string{"internal"},
		},
		{
			Id: "faa_part_107",
			Title: "Small Unmanned Aircraft Systems - 14 CFR Part 107",
			Content: "Federal Aviation Administration regulations for small unmanned aircraft systems...",
			Url: "https://docs.company.com/regulations/faa-part107.pdf",
			Version: "2021",
			ClearanceLevels: []string{"public"},
		},
		{
			Id: "sop_001",
			Title: "Carbon Fiber Frame Inspection - SOP-001",
			Content: "Standard Operating Procedure for inspecting carbon fiber frames...",
			Url: "https://docs.company.com/procedures/sop-001-frame-inspection.pdf",
			Version: "3.1",
			ClearanceLevels: []string{"internal"},
		},
		{
			Id: "wi_motor_torque",
			Title: "Motor Mounting Torque Specifications - WI-015",
			Content: "Work instruction for proper motor mounting torque specifications...",
			Url: "https://docs.company.com/instructions/wi-015-motor-torque.pdf",
			Version: "2.0",
			ClearanceLevels: []string{"internal"},
		},
		{
			Id: "safety_esd",
			Title: "Electrostatic Discharge Prevention - SAFE-001",
			Content: "Safety procedures for preventing electrostatic discharge during assembly...",
			Url: "https://docs.company.com/safety/esd-prevention.pdf",
			Version: "1.5",
			ClearanceLevels: []string{"internal"},
		},
		{
			Id: "qc_electrical",
			Title: "Electrical System Testing Protocol - QC-TEST-003",
			Content: "Quality control procedures for electrical system testing...",
			Url: "https://docs.company.com/quality/electrical-testing.pdf",
			Version: "4.2",
			ClearanceLevels: []string{"restricted"},
		},
		{
			Id: "spec_frame",
			Title: "QC-200 Frame Specification",
			Content: "Technical specifications for the QC-200 drone frame...",
			Url: "https://docs.company.com/specs/qc200-frame-spec.pdf",
			Version: "C",
			ClearanceLevels: []string{"restricted"},
		},
		{
			Id: "dwg_frame",
			Title: "Carbon Fiber Frame Assembly Drawing",
			Content: "Technical drawing for carbon fiber frame assembly...",
			Url: "https://docs.company.com/drawings/qc200-frame-assembly.pdf",
			Version: "C",
			ClearanceLevels: []string{"restricted"},
		},
		{
			Id: "proprietary_design",
			Title: "Proprietary Flight Control Algorithm",
			Content: "Confidential flight control algorithm specifications...",
			Url: "https://docs.company.com/confidential/flight-control-algo.pdf",
			Version: "1.0",
			ClearanceLevels: []string{"confidential"},
		},
	}

	for _, doc := range sampleDocs {
		s.documents[doc.Id] = doc
		
		// Store in Redis
		docData, err := json.Marshal(doc)
		if err != nil {
			s.logger.Error("Failed to serialize sample document", err, watermill.LogFields{
				"document_id": doc.Id,
			})
			continue
		}

		err = s.redisClient.Set(context.Background(), fmt.Sprintf("document:%s", doc.Id), docData, 0).Err()
		if err != nil {
			s.logger.Error("Failed to store sample document in Redis", err, watermill.LogFields{
				"document_id": doc.Id,
			})
		}
	}

	s.logger.Info("Initialized sample documents", watermill.LogFields{
		"document_count": len(sampleDocs),
	})
}

// Helper method to get document by type
func (s *Service) GetDocumentsByType(ctx context.Context, docType string) []*pb.Document {
	s.mu.RLock()
	defer s.mu.RUnlock()

	var docs []*pb.Document
	for _, doc := range s.documents {
		// Simple type matching based on document ID patterns
		if s.matchesDocumentType(doc.Id, docType) {
			docs = append(docs, doc)
		}
	}

	return docs
}

func (s *Service) matchesDocumentType(docID, docType string) bool {
	switch docType {
	case "standard":
		return strings.Contains(docID, "iso") || strings.Contains(docID, "faa") || strings.Contains(docID, "ce")
	case "procedure":
		return strings.Contains(docID, "sop") || strings.Contains(docID, "wi") || strings.Contains(docID, "safety") || strings.Contains(docID, "qc")
	case "specification":
		return strings.Contains(docID, "spec")
	case "drawing":
		return strings.Contains(docID, "dwg")
	default:
		return false
	}
}

