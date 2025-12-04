CREATE TABLE `prolog_presets` (
	`id` int AUTO_INCREMENT NOT NULL,
	`name` varchar(255) NOT NULL,
	`description` text,
	`category` varchar(100) NOT NULL,
	`facts` text NOT NULL,
	`exampleQueries` text NOT NULL,
	`createdAt` timestamp NOT NULL DEFAULT (now()),
	CONSTRAINT `prolog_presets_id` PRIMARY KEY(`id`)
);
--> statement-breakpoint
CREATE TABLE `prolog_sessions` (
	`id` int AUTO_INCREMENT NOT NULL,
	`userId` int,
	`name` varchar(255) NOT NULL,
	`facts` text NOT NULL,
	`description` text,
	`createdAt` timestamp NOT NULL DEFAULT (now()),
	`updatedAt` timestamp NOT NULL DEFAULT (now()) ON UPDATE CURRENT_TIMESTAMP,
	CONSTRAINT `prolog_sessions_id` PRIMARY KEY(`id`)
);
--> statement-breakpoint
ALTER TABLE `prolog_sessions` ADD CONSTRAINT `prolog_sessions_userId_users_id_fk` FOREIGN KEY (`userId`) REFERENCES `users`(`id`) ON DELETE no action ON UPDATE no action;