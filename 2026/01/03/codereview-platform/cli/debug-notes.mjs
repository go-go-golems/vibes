import { createGitNotesStorage, GIT_NOTES_REFS } from '../server/lib/git-notes.js';

const repoPath = '/tmp/code-review-demo/demo-codebase';
const storage = createGitNotesStorage(repoPath);

console.log('Refs:', GIT_NOTES_REFS);

const notes = await storage.listNotes(GIT_NOTES_REFS.REVIEWS);
console.log('Notes:', notes);

if (notes.length > 0) {
  const review = await storage.getReview(notes[0].commit);
  console.log('First review:', review?.title);
}

const reviews = await storage.getAllReviews();
console.log('Total reviews:', reviews.length);
