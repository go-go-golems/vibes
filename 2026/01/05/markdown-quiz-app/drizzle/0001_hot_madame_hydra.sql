CREATE TABLE `documents` (
	`id` int AUTO_INCREMENT NOT NULL,
	`title` varchar(255) NOT NULL,
	`slug` varchar(255) NOT NULL,
	`content` text NOT NULL,
	`description` text,
	`category` varchar(100),
	`isPublished` boolean NOT NULL DEFAULT false,
	`authorId` int NOT NULL,
	`createdAt` timestamp NOT NULL DEFAULT (now()),
	`updatedAt` timestamp NOT NULL DEFAULT (now()) ON UPDATE CURRENT_TIMESTAMP,
	CONSTRAINT `documents_id` PRIMARY KEY(`id`),
	CONSTRAINT `documents_slug_unique` UNIQUE(`slug`)
);
--> statement-breakpoint
CREATE TABLE `quiz_forms` (
	`id` int AUTO_INCREMENT NOT NULL,
	`documentId` int NOT NULL,
	`formId` varchar(100) NOT NULL,
	`definition` json NOT NULL,
	`createdAt` timestamp NOT NULL DEFAULT (now()),
	`updatedAt` timestamp NOT NULL DEFAULT (now()) ON UPDATE CURRENT_TIMESTAMP,
	CONSTRAINT `quiz_forms_id` PRIMARY KEY(`id`)
);
--> statement-breakpoint
CREATE TABLE `quiz_submissions` (
	`id` int AUTO_INCREMENT NOT NULL,
	`userId` int NOT NULL,
	`documentId` int NOT NULL,
	`formId` varchar(100) NOT NULL,
	`responses` json NOT NULL,
	`score` int,
	`maxScore` int,
	`submittedAt` timestamp NOT NULL DEFAULT (now()),
	CONSTRAINT `quiz_submissions_id` PRIMARY KEY(`id`)
);
