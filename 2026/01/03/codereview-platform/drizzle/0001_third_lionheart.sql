CREATE TABLE `annotations` (
	`id` int AUTO_INCREMENT NOT NULL,
	`repositoryId` int NOT NULL,
	`codeReviewId` int,
	`filePath` varchar(512) NOT NULL,
	`lineNumber` int,
	`lineEnd` int,
	`annotationType` enum('educational','knowledge_share','pattern_highlight','gotcha','evolution','question') DEFAULT 'educational',
	`title` varchar(256),
	`content` text,
	`tags` json,
	`relatedQuizId` int,
	`metadata` json,
	`gitNotesRef` varchar(256),
	`createdAt` timestamp NOT NULL DEFAULT (now()),
	CONSTRAINT `annotations_id` PRIMARY KEY(`id`)
);
--> statement-breakpoint
CREATE TABLE `code_reviews` (
	`id` int AUTO_INCREMENT NOT NULL,
	`repositoryId` int NOT NULL,
	`prNumber` int,
	`title` varchar(512) NOT NULL,
	`description` text,
	`status` enum('draft','open','merged','closed') NOT NULL DEFAULT 'open',
	`baseBranch` varchar(128),
	`headBranch` varchar(128),
	`baseCommit` varchar(64),
	`headCommit` varchar(64),
	`authorId` int,
	`gitNotesRef` varchar(256),
	`createdAt` timestamp NOT NULL DEFAULT (now()),
	`updatedAt` timestamp NOT NULL DEFAULT (now()) ON UPDATE CURRENT_TIMESTAMP,
	CONSTRAINT `code_reviews_id` PRIMARY KEY(`id`)
);
--> statement-breakpoint
CREATE TABLE `guide_progress` (
	`id` int AUTO_INCREMENT NOT NULL,
	`userId` int NOT NULL,
	`guideId` int NOT NULL,
	`repositoryId` int NOT NULL,
	`currentStopId` varchar(128),
	`visitedStops` json,
	`completedAt` timestamp,
	`startedAt` timestamp NOT NULL DEFAULT (now()),
	`updatedAt` timestamp NOT NULL DEFAULT (now()) ON UPDATE CURRENT_TIMESTAMP,
	CONSTRAINT `guide_progress_id` PRIMARY KEY(`id`)
);
--> statement-breakpoint
CREATE TABLE `quiz_submissions` (
	`id` int AUTO_INCREMENT NOT NULL,
	`userId` int NOT NULL,
	`quizId` int NOT NULL,
	`repositoryId` int NOT NULL,
	`answers` json,
	`score` int,
	`maxScore` int,
	`completed` boolean DEFAULT false,
	`gitNotesRef` varchar(256),
	`submittedAt` timestamp NOT NULL DEFAULT (now()),
	CONSTRAINT `quiz_submissions_id` PRIMARY KEY(`id`)
);
--> statement-breakpoint
CREATE TABLE `quizzes` (
	`id` int AUTO_INCREMENT NOT NULL,
	`repositoryId` int NOT NULL,
	`codeReviewId` int,
	`quizId` varchar(128) NOT NULL,
	`title` varchar(512) NOT NULL,
	`description` text,
	`filePath` varchar(512),
	`lineStart` int,
	`lineEnd` int,
	`difficulty` enum('beginner','intermediate','advanced') DEFAULT 'intermediate',
	`estimatedTime` varchar(32),
	`gitNotesRef` varchar(256),
	`createdAt` timestamp NOT NULL DEFAULT (now()),
	`updatedAt` timestamp NOT NULL DEFAULT (now()) ON UPDATE CURRENT_TIMESTAMP,
	CONSTRAINT `quizzes_id` PRIMARY KEY(`id`)
);
--> statement-breakpoint
CREATE TABLE `repositories` (
	`id` int AUTO_INCREMENT NOT NULL,
	`name` varchar(255) NOT NULL,
	`path` varchar(512) NOT NULL,
	`description` text,
	`defaultBranch` varchar(128) DEFAULT 'main',
	`createdAt` timestamp NOT NULL DEFAULT (now()),
	`updatedAt` timestamp NOT NULL DEFAULT (now()) ON UPDATE CURRENT_TIMESTAMP,
	CONSTRAINT `repositories_id` PRIMARY KEY(`id`),
	CONSTRAINT `repositories_path_unique` UNIQUE(`path`)
);
--> statement-breakpoint
CREATE TABLE `review_guides` (
	`id` int AUTO_INCREMENT NOT NULL,
	`repositoryId` int NOT NULL,
	`codeReviewId` int,
	`guideId` varchar(128) NOT NULL,
	`title` varchar(512) NOT NULL,
	`description` text,
	`difficulty` enum('beginner','intermediate','advanced') DEFAULT 'intermediate',
	`estimatedTime` varchar(32),
	`prerequisites` json,
	`gitNotesRef` varchar(256),
	`createdAt` timestamp NOT NULL DEFAULT (now()),
	`updatedAt` timestamp NOT NULL DEFAULT (now()) ON UPDATE CURRENT_TIMESTAMP,
	CONSTRAINT `review_guides_id` PRIMARY KEY(`id`)
);
